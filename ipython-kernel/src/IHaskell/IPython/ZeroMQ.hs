{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, FlexibleContexts, CPP #-}

-- | Description : Low-level ZeroMQ communication wrapper.
--
-- The "ZeroMQ" module abstracts away the low-level 0MQ based interface with IPython, replacing it
-- instead with a Haskell Channel based interface. The `serveProfile` function takes a IPython
-- profile specification and returns the channel interface to use.
module IHaskell.IPython.ZeroMQ (
    ZeroMQInterface(..),
    ZeroMQStdin(..),
    serveProfile,
    serveStdin,
    ZeroMQEphemeralPorts,
    withEphemeralPorts,
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (SHA256)
import qualified Crypto.MAC.HMAC as HMAC
import           Data.Aeson
import qualified Data.ByteArray.Encoding as Encoding
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as Text
import           System.ZMQ4 as ZMQ4
import           Text.Read (readMaybe)
import           Text.Parsec (runParserT, manyTill, anyToken, (<|>), eof, tokenPrim, incSourceColumn)

import           IHaskell.IPython.Message.Parser
import           IHaskell.IPython.Types

-- | The channel interface to the ZeroMQ sockets. All communication is done via Messages, which are
-- encoded and decoded into a lower level form before being transmitted to IPython. These channels
-- should functionally serve as high-level sockets which speak Messages instead of ByteStrings.
data ZeroMQInterface =
       Channels
         {
         -- | A channel populated with requests from the frontend.
         -- Shared between the shell and control channels.
         -- The channel content is a tuple (replyChannel, message):
         --   * If the message comes from the shell channel, 'replyChannel' is the shell reply channel.
         --   * If the message comes from the control channel, 'replyChannel' is the control reply channel.
         shellRequestChannel :: Chan (Chan Message, Message)
         -- | Writing to this channel causes a reply to be sent to the frontend.
         , shellReplyChannel :: Chan Message
         -- | This channel is a duplicate of the shell request channel, though using a different backend
         -- socket.
         , controlRequestChannel :: Chan (Chan Message, Message)
         -- | This channel is a duplicate of the shell reply channel, though using a different backend
         -- socket.
         , controlReplyChannel :: Chan Message
         -- | Writing to this channel sends an iopub message to the frontend.
         , iopubChannel :: Chan Message
         -- | Key used to sign messages.
         , hmacKey :: ByteString
         }

data ZeroMQStdin =
       StdinChannel
         { stdinRequestChannel :: Chan Message
         , stdinReplyChannel :: Chan Message
         }

-- | Create new channels for a ZeroMQInterface
newZeroMQInterface :: ByteString -> IO ZeroMQInterface
newZeroMQInterface key = do
  shellReqChan <- newChan
  shellRepChan <- newChan
  controlReqChan <- dupChan shellReqChan
  controlRepChan <- newChan
  iopubChan <- newChan
  return $! Channels
    { shellRequestChannel = shellReqChan
    , shellReplyChannel = shellRepChan
    , controlRequestChannel = controlReqChan
    , controlReplyChannel = controlRepChan
    , iopubChannel = iopubChan
    , hmacKey = key
    }

-- | Start responding on all ZeroMQ channels used to communicate with IPython | via the provided
-- profile. Return a set of channels which can be used to | communicate with IPython in a more
-- structured manner.
serveProfile :: Profile            -- ^ The profile specifying which ports and transport mechanisms to use.
             -> Bool               -- ^ Print debug output
             -> IO ZeroMQInterface -- ^ The Message-channel based interface to the sockets.
serveProfile profile debug = do
  channels <- newZeroMQInterface (signatureKey profile)

  -- Create the context in a separate thread that never finishes. If withContext or withSocket
  -- complete, the context or socket become invalid.
  _ <- forkIO $ withContext $ \ctxt -> do
    -- Serve on all sockets.
    _ <- forkIO $ serveSocket ctxt Rep (ip profile) (hbPort profile) $ heartbeat channels
    _ <- forkIO $ serveSocket ctxt Router (ip profile) (controlPort profile) $ control debug channels
    _ <- forkIO $ serveSocket ctxt Router (ip profile) (shellPort profile) $ shell debug channels

    -- The ctxt is reference counted in this thread only. Thus, the last serveSocket cannot be
    -- asynchronous, because otherwise ctxt would be garbage collectable - since it would only be
    -- used in other threads. Thus, keep the last serveSocket in this thread.
    serveSocket ctxt Pub (ip profile) (iopubPort profile) $ iopub debug channels

  return channels

-- | Describes ports used when creating an ephemeral ZeroMQ session. Used to generate the ipython
-- JSON config file.
data ZeroMQEphemeralPorts =
       ZeroMQEphemeralPorts
         { ephHbPort :: !Port
         , ephControlPort :: !Port
         , ephShellPort :: !Port
         , ephIOPubPort :: !Port
         , ephSignatureKey :: !ByteString
         }

instance ToJSON ZeroMQEphemeralPorts where
  toJSON ports =
    object
      [ "ip" .= ("127.0.0.1" :: String)
      , "transport" .= TCP
      , "control_port" .= ephControlPort ports
      , "hb_port" .= ephHbPort ports
      , "shell_port" .= ephShellPort ports
      , "iopub_port" .= ephIOPubPort ports
      , "key" .= Text.decodeUtf8 (ephSignatureKey ports)
      ]

parsePort :: String -> Maybe Int
parsePort s = readMaybe num
  where
    num = reverse (takeWhile isNumber (reverse s))

bindLocalEphemeralPort :: Socket a -> IO Int
bindLocalEphemeralPort sock = do
  bind sock $ "tcp://127.0.0.1:*"
  endpointString <- lastEndpoint sock
  case parsePort endpointString of
    Nothing ->
      fail $ "internalError: IHaskell.IPython.ZeroMQ.bindLocalEphemeralPort encountered a port index that could not be interpreted as an int."
    Just endpointIndex ->
      return endpointIndex

-- | Run session for communicating with an IPython instance on ephemerally allocated ZMQ4 sockets.
-- The sockets will be closed when the callback returns.
withEphemeralPorts :: ByteString -- ^ HMAC encryption key
                   -> Bool -- ^ Print debug output
                   -> (ZeroMQEphemeralPorts
                    -> ZeroMQInterface
                    -> IO a) -- ^ Callback that takes the interface to the sockets.
                   -> IO a
withEphemeralPorts key debug callback = do
  channels <- newZeroMQInterface key
  -- Create the ZMQ4 context
  withContext $ \ctxt -> do
    -- Create the sockets to communicate with.
    withSocket ctxt Rep $ \heartbeatSocket -> do
      withSocket ctxt Router $ \controlportSocket -> do
        withSocket ctxt Router $ \shellportSocket -> do
          withSocket ctxt Pub $ \iopubSocket -> do
            -- Bind each socket to a local port, getting the port chosen.
            hbPt <- bindLocalEphemeralPort heartbeatSocket
            controlPt <- bindLocalEphemeralPort controlportSocket
            shellPt <- bindLocalEphemeralPort shellportSocket
            iopubPt <- bindLocalEphemeralPort iopubSocket
            -- Create object to store ephemeral ports
            let ports = ZeroMQEphemeralPorts hbPt controlPt shellPt iopubPt key
            -- Launch actions to listen to communicate between channels and cockets.
            _ <- forkIO $ forever $ heartbeat channels heartbeatSocket
            _ <- forkIO $ forever $ control debug channels controlportSocket
            _ <- forkIO $ forever $ shell debug channels shellportSocket
            _ <- forkIO $ checkedIOpub debug channels iopubSocket
            -- Run callback function; provide it with both ports and channels.
            callback ports channels

serveStdin :: Profile -> IO ZeroMQStdin
serveStdin profile = do
  reqChannel <- newChan
  repChannel <- newChan

  -- Create the context in a separate thread that never finishes. If withContext or withSocket
  -- complete, the context or socket become invalid.
  _ <- forkIO $ withContext $ \ctxt ->
    -- Serve on all sockets.
    serveSocket ctxt Router (ip profile) (stdinPort profile) $ \sock -> do
      -- Read the request from the interface channel and send it.
      readChan reqChannel >>= sendMessage False (signatureKey profile) sock

      -- Receive a response and write it to the interface channel.
      receiveMessage False sock >>= writeChan repChannel

  return $ StdinChannel reqChannel repChannel

-- | Serve on a given sock in a separate thread. Bind the sock in the | given context and then
-- loop the provided action, which should listen | on the sock and respond to any events.
serveSocket :: SocketType a => Context -> a -> IP -> Port -> (Socket a -> IO b) -> IO ()
serveSocket ctxt socketType ipAddress port action = void $
  withSocket ctxt socketType $ \sock -> do
    bind sock $ "tcp://" ++ ipAddress ++ ":" ++ show port
    forever $ action sock

-- | Listener on the heartbeat port. Echoes back any data it was sent.
heartbeat :: ZeroMQInterface -> Socket Rep -> IO ()
heartbeat _ sock = do
  -- Read some data.
  request <- receive sock

  -- Send it back.
  send sock [] request

-- | Listener on the shell port. Reads messages and writes them to | the shell request channel. For
-- each message, reads a response from the | shell reply channel of the interface and sends it back
-- to the frontend.
shell :: Bool -> ZeroMQInterface -> Socket Router -> IO ()
shell debug channels sock = do
  -- Receive a message and write it to the interface channel.
  message <- receiveMessage debug sock
  writeChan requestChannel (replyChannel, message)

  -- Read the reply from the interface channel and send it.
  readChan replyChannel >>= sendMessage debug (hmacKey channels) sock

  where
    requestChannel = shellRequestChannel channels
    replyChannel = shellReplyChannel channels

-- | Listener on the shell port. Reads messages and writes them to | the shell request channel. For
-- each message, reads a response from the | shell reply channel of the interface and sends it back
-- to the frontend.
control :: Bool -> ZeroMQInterface -> Socket Router -> IO ()
control debug channels sock = do
  -- Receive a message and write it to the interface channel.
  message <- receiveMessage debug sock
  writeChan requestChannel (replyChannel, message)

  -- Read the reply from the interface channel and send it.
  readChan replyChannel >>= sendMessage debug (hmacKey channels) sock

  where
    requestChannel = controlRequestChannel channels
    replyChannel = controlReplyChannel channels

-- | Send messages via the iopub channel. | This reads messages from the ZeroMQ iopub interface
-- channel | and then writes the messages to the socket.
iopub :: Bool -> ZeroMQInterface -> Socket Pub -> IO ()
iopub debug channels sock =
  readChan (iopubChannel channels) >>= sendMessage debug (hmacKey channels) sock

-- | Attempt to send a message along the socket, returning true if successful.
trySendMessage :: Sender a => String -> Bool -> ByteString -> Socket a -> Message -> IO Bool
trySendMessage _ debug hmackey sock msg = do
  let zmqErrorHandler :: ZMQError -> IO Bool
      zmqErrorHandler e
        -- Ignore errors if we cannot send. We may want to forward this to the thread that tried put the
        -- message in the Chan initially.
        | errno e == 38 = return False
        | otherwise = throwIO e
  (sendMessage debug hmackey sock msg >> return True) `catch` zmqErrorHandler

-- | Send messages via the iopub channel. This reads messages from the ZeroMQ iopub interface
-- channel and then writes the messages to the socket. This is a checked implementation which will
-- stop if the socket is closed.
checkedIOpub :: Bool -> ZeroMQInterface -> Socket Pub -> IO ()
checkedIOpub debug channels sock = do
  msg <- readChan (iopubChannel channels)
  cont <- trySendMessage "io" debug (hmacKey channels) sock msg
  when cont $
    checkedIOpub debug channels sock

-- | Receive and parse a message from a socket.
receiveMessage :: Receiver a => Bool -> Socket a -> IO Message
receiveMessage debug sock = do
  blobs <- receiveMulti sock
  runParserT parseBlobs () "" blobs >>= \r -> case r of
    Left parseerr -> fail $ "Malformed Wire Protocol message: " <> show parseerr
    Right (idents, headerData, parentHeader, metaData, content, buffers) -> do
      when debug $ do
        putStr "Header: "
        Char.putStrLn headerData
        putStr "Content: "
        Char.putStrLn content
      return $ parseMessage idents headerData parentHeader metaData content buffers
  where
    parseBlobs = do
      idents       <- manyTill anyToken (satisfy (=="<IDS|MSG>"))
      _            <- anyToken <|> fail "No signature"
      headerData   <- anyToken <|> fail "No headerData"
      parentHeader <- anyToken <|> fail "No parentHeader"
      metaData     <- anyToken <|> fail "No metaData"
      content      <- anyToken <|> fail "No contents"
      buffers      <- manyTill anyToken eof
      pure (idents, headerData, parentHeader, metaData, content, buffers)
    satisfy f = tokenPrim Char.unpack (\pos _ _ -> incSourceColumn pos 1)
                                      (\c -> if f c then Just c else Nothing)

-- | Encode a message in the IPython ZeroMQ communication protocol and send it through the provided
-- socket. Sign it using HMAC with SHA-256 using the provided key.
sendMessage :: Sender a => Bool -> ByteString -> Socket a -> Message -> IO ()
sendMessage _ _ _ SendNothing = return ()
sendMessage debug hmackey sock msg = do
  when debug $ do
    putStr "Message: "
    print msg
    putStr "Sent: "
    print content

  -- Send all pieces of the message.
  mapM_ sendPiece idents
  sendPiece "<IDS|MSG>"
  sendPiece signature
  sendPiece headStr
  sendPiece parentHeaderStr
  sendPiece metadata

  -- If there are no mhBuffers, then conclude transmission with content.
  case mhBuffers hdr of
    [] -> sendLast content
    _  -> sendPiece content

  sendBuffers $ mhBuffers hdr

  where
    sendBuffers [] = pure ()
    sendBuffers [b] = sendLast b
    sendBuffers (b:bs) = sendPiece b >> sendBuffers bs

    sendPiece = send sock [SendMore]
    sendLast = send sock []

    -- Encode to a strict bytestring.
    encodeStrict :: ToJSON a => a -> ByteString
    encodeStrict = LBS.toStrict . encode

    -- Signature for the message using HMAC SHA-256.
    signature :: ByteString
    signature = hmac $ headStr <> parentHeaderStr <> metadata <> content

    -- Compute the HMAC SHA-256 signature of a bytestring message.
    hmac :: ByteString -> ByteString
    hmac = (Encoding.convertToBase Encoding.Base16 :: Hash.Digest SHA256 -> ByteString)
      . HMAC.hmacGetDigest
      . HMAC.hmac hmackey

    -- Pieces of the message.
    hdr = header msg
    parentHeaderStr = maybe "{}" encodeStrict $ mhParentHeader hdr
    idents = mhIdentifiers hdr
    metadata = let Metadata mdobject = mhMetadata hdr in encodeStrict mdobject
    content = encodeStrict msg
    headStr = encodeStrict hdr
