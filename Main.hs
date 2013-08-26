import BasicPrelude
import Control.Concurrent
import Control.Monad.Trans.State
import Data.Aeson
import Data.UUID.V4 (nextRandom)
import System.Posix.Process
import System.ZMQ
import Text.Printf

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.MessageParser


stdin :: Int
stdin = 5678

kernelProfile :: Profile
kernelProfile = Profile {
  ip = "127.0.0.1",
  transport = "tcp",
  stdinPort = stdin,
  controlPort = stdin+1,
  hbPort = stdin+2,
  shellPort = stdin+3,
  iopubPort = stdin+4,
  key = ""
  }

main ::  IO ()
main = do
  putStrLn "Arguments:"
  getArgs >>= print 

  pid <- getProcessID
  let fname = printf "profile-%s.json" $ textToString $ show pid
  ByteString.writeFile fname $ encode kernelProfile
  withContext 1 $ serveProfile kernelProfile

serveProfile :: Profile -> Context -> IO ()
serveProfile profile context = do
 waitVar <- newEmptyMVar 
 commandChannel <- newChan
 replyChan <- newChan
 putStrLn "Starting server..."
 serveSocket context Router (stdinPort profile) stdinAction
 serveSocket context Router (controlPort profile) $ shellAction commandChannel replyChan
 serveSocket context Rep    (hbPort profile) hbAction
 serveSocket context Router (shellPort profile) $ shellAction commandChannel replyChan
 serveSocket context Pub    (iopubPort profile) iopubAction
 putStrLn "Serving...."

 let kernelState = 0
     writeReply request reply = do
       messageId <- nextRandom
       let parent = header request
           msgHead = MessageHeader {
         identifiers = identifiers parent,
         parentHeader = Just parent,
         metadata = Map.fromList [],
         messageId = messageId,
         sessionId = sessionId parent,
         username = username parent,
         msgType = messageTypeForBody reply
         }
       let msg = Message {
         header = msgHead,
         body = reply
         }
       writeChan replyChan msg
     evalMessage :: Int -> Message -> IO Int
     evalMessage st message =
       let (reply, newState) = runState (processMessage (body message)) st in do
         putStr $ fromString $ printf "Responses: %d\n" newState 
         writeReply message reply
         return newState

 messages <- getChanContents commandChannel
 foldM_ evalMessage kernelState messages

 takeMVar waitVar

processMessage :: MessageBody -> State Int MessageBody
processMessage KernelInfoRequest = do
  modify (+1)
  return KernelInfoReply
processMessage msg = error $ "Unknown message type " ++ textToString (show msg) 
  

serveSocket :: SType a => Context -> a -> Port -> (Socket a -> IO b) -> IO ()
serveSocket context socketType port action = void . forkIO $
  withSocket context socketType $ \socket -> do
    bind socket $ textToString $ "tcp://127.0.0.1:" ++ show port
    forever $ action socket

stdinAction :: Socket a -> IO ()
stdinAction socket = do
  request <- receive socket []
  putStrLn "Received stdin."
  print request

hbAction :: Socket a -> IO ()
hbAction socket = do
  request <- receive socket []
  putStrLn "Received heartbeat."
  print request
  -- echo back request
  send socket request []

shellAction :: Chan Message -> Chan Message -> Socket a -> IO ()
shellAction channel readchan socket = do
  msg <- ipythonReceive socket
  writeChan channel msg
  reply <- readChan readchan
  ipythonSend socket reply

iopubAction :: Socket a -> IO ()
iopubAction socket = do
  request <- receive socket []
  putStrLn "Received iopub."
  print request

ipythonReceive :: Socket a -> IO Message
ipythonReceive socket = do
  let receiveNext = receive socket []
      readMessage = do
        line <- receiveNext
        if line /= "<IDS|MSG>"
        then do
          next <- readMessage
          return $ line : next
        else return []
  
  idents <- readMessage
  signature <- receive socket [] -- signature
  headerData <- receive socket []
  parentHeader <- receive socket []
  metadata <- receive socket []
  content <- receive socket []
  putStrLn "ipython receive"
  print idents
  print signature
  print headerData
  print parentHeader
  print metadata
  print content
  putStrLn "receive done"

  return $ parseMessage idents headerData parentHeader metadata content

ipythonSend :: Socket a -> Message -> IO ()
ipythonSend socket message = 
  let msgHead = header message
      parentHeaderStr = case parentHeader msgHead of
                          Nothing -> "{}"
                          Just parentHead -> encode parentHead
      idents = identifiers msgHead
      metadata = "{}"
      content = encode $ body message in do

  let headerStr = ByteString.toStrict $ encode msgHead
  putStrLn "ipython send"
  forM_ idents $ \ident -> print ident
  putStrLn "<IDS|MSG>"
  putStrLn ""
  print headerStr
  print (ByteString.toStrict parentHeaderStr)
  print metadata
  print (ByteString.toStrict content)
  putStrLn "send done"

  forM_ idents $ \ident -> send socket ident [SndMore]
  send socket "<IDS|MSG>" [SndMore]
  send socket "" [SndMore]
  send socket headerStr [SndMore]
  send socket (ByteString.toStrict parentHeaderStr) [SndMore]
  send socket metadata [SndMore]
  send socket (ByteString.toStrict content) []

