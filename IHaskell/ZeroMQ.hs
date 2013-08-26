module IHaskell.ZeroMQ where

import BasicPrelude
import Control.Concurrent
import System.ZMQ
import Data.Aeson (encode, ToJSON)

import qualified Data.ByteString.Lazy as ByteString

import Debug.Trace

import IHaskell.Types
import IHaskell.MessageParser

data ZeroMQInterface = Channels {
  shellRequestChannel :: Chan Message,
  shellReplyChannel :: Chan Message,
  controlRequestChannel :: Chan Message,
  controlReplyChannel :: Chan Message
  }

-- | Start responding on all ZeroMQ channels used to communicate with IPython
-- | via the provided profile. Return a set of channels which can be used to
-- | communicate with IPython in a more structured manner. 
serveProfile :: Profile -> IO ZeroMQInterface
serveProfile profile = do
  -- Create all channels which will be used for higher level communication.
  shellReqChan <- newChan
  shellRepChan <- newChan
  controlReqChan <- dupChan shellReqChan
  controlRepChan <- dupChan shellRepChan
  let channels = Channels shellReqChan shellRepChan controlReqChan controlRepChan

  -- Create the context in a separate thread that never finishes. If
  -- withContext or withSocket complete, the context or socket become invalid.
  forkIO $ withContext 1 $ \context -> do
    -- Serve on all sockets.
    serveSocket context Rep    (hbPort profile)      $ heartbeat channels
    serveSocket context Router (controlPort profile) $ control   channels
    serveSocket context Router (shellPort profile)   $ shell     channels
    serveSocket context Router (stdinPort profile)   $ stdin     channels
    serveSocket context Pub    (iopubPort profile)   $ iopub     channels

    -- Wait forever
    newEmptyMVar >>= takeMVar

  return channels

-- | Serve on a given socket in a separate thread. Bind the socket in the
-- | given context and then loop the provided action, which should listen
-- | on the socket and respond to any events.
serveSocket :: SType a => Context -> a -> Port -> (Socket a -> IO b) -> IO ()
serveSocket context socketType port action = void . forkIO $
  withSocket context socketType $ \socket -> do
    bind socket $ textToString $ "tcp://127.0.0.1:" ++ show port
    forever $ action socket

-- | Listener on the heartbeat port. Echoes back any data it was sent.
heartbeat :: ZeroMQInterface -> Socket Rep -> IO ()
heartbeat _ socket = do
  -- Read some data.
  request <- receive socket []

  -- Send it back.
  send socket request []

-- | Listener on the shell port. Reads messages and writes them to
-- | the shell request channel. For each message, reads a response from the
-- | shell reply channel of the interface and sends it back to the frontend. 
shell :: ZeroMQInterface -> Socket Router -> IO ()
shell channels socket = do
  -- Receive a message and write it to the interface channel.
  receiveMessage socket >>= writeChan requestChannel

  -- Read the reply from the interface channel and send it.
  readChan replyChannel >>= sendMessage socket

  where
    requestChannel = shellRequestChannel channels
    replyChannel = shellReplyChannel channels

-- | Listener on the shell port. Reads messages and writes them to
-- | the shell request channel. For each message, reads a response from the
-- | shell reply channel of the interface and sends it back to the frontend. 
control :: ZeroMQInterface -> Socket Router -> IO ()
control channels socket = do
  -- Receive a message and write it to the interface channel.
  receiveMessage socket >>= writeChan requestChannel

  -- Read the reply from the interface channel and send it.
  readChan replyChannel >>= sendMessage socket

  where
    requestChannel = controlRequestChannel channels
    replyChannel =   controlReplyChannel channels

stdin :: ZeroMQInterface -> Socket Router -> IO ()
stdin _ socket = do
  next <- receive socket []
  print next

iopub :: ZeroMQInterface -> Socket Pub -> IO ()
iopub _ socket = do
  next <- receive socket []
  print next

-- | Receive and parse a message from a socket.
receiveMessage :: Socket a -> IO Message
receiveMessage socket = do
  idents <- readUntil "<IDS|MSG>"

  -- Ignore the signature for now.
  void next

  headerData <- next
  parentHeader <- next
  metadata <- next
  content <- next

  return $ parseMessage idents headerData parentHeader metadata content

  where
    -- Receive the next piece of data from the socket.
    next = receive socket []

    -- Read data from the socket until we hit an ending string.
    -- Return all data as a list, which does not include the ending string.
    readUntil str = do
      line <- next
      if line /= str
      then do
        remaining <- readUntil str
        return $ line : remaining
      else return []
  

sendMessage :: Socket a -> Message -> IO ()
sendMessage socket message = do
  let head = header message
      parentHeaderStr = maybe "{}" encodeLazy $ parentHeader head
      idents = identifiers head
      metadata = "{}"
      content = encodeLazy message
      headStr = encodeLazy head

  -- Send all pieces of the message.
  mapM_ sendPiece idents
  sendPiece "<IDS|MSG>"
  sendPiece ""
  sendPiece headStr
  sendPiece parentHeaderStr
  sendPiece metadata

  -- Conclude transmission with content.
  sendLast (trace (textToString $ show content) content)

  where
    sendPiece str = send socket str [SndMore]
    sendLast str = send socket str []

    encodeLazy :: ToJSON a => a -> ByteString
    encodeLazy = ByteString.toStrict . encode
