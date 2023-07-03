module TurtleProtocol.Client where

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TQueue qualified as TQueue
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Streaming.Network qualified as Network
import Ki.Unlifted qualified as Ki
import Relude
import TurtleProtocol.Message (Request, Response)

data ResponseHandler a where
  IgnoreResponse :: ResponseHandler ()
  AwaitResponse :: (Response -> Maybe a) -> ResponseHandler a

data Connection = Connection
  { remainderReference :: IORef ByteString,
    socket :: Network.AppDataUnix,
    requestQueue :: TQueue Request,
    responseHandlerQueue :: TQueue (Response -> IO ())
  }

newtype Client a = Client (Connection -> IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail)
    via ReaderT Connection IO

runUnix :: (MonadIO m) => FilePath -> Client a -> m a
runUnix socketPath (Client f) =
  liftIO $ Ki.scoped \threadScope -> do
    remainderReference <- newIORef ByteString.empty
    requestQueue <- TQueue.newTQueueIO
    responseHandlerQueue <- TQueue.newTQueueIO

    Network.runUnixClient (Network.clientSettingsUnix socketPath) \socket -> do
      let connection =
            Connection
              { remainderReference,
                socket,
                requestQueue,
                responseHandlerQueue
              }

      applicationThread <- Ki.fork threadScope (f connection)
      _ <- Ki.fork threadScope (sendRequests connection)
      _ <- Ki.fork threadScope (handleResponses connection)
      atomically $ Ki.await applicationThread

sendRequests :: Connection -> IO a
sendRequests Connection {socket, requestQueue} =
  forever do
    request <- atomically $ TQueue.readTQueue requestQueue
    traverse_ (liftIO . Network.appWrite socket) $
      LazyByteString.toChunks (Binary.runPut (Binary.put @Request request))

handleResponses :: forall a. Connection -> IO a
handleResponses Connection {socket, responseHandlerQueue, remainderReference} = do
  let readMessage :: forall m. (MonadIO m) => m Response
      readMessage =
        let go :: Binary.Decoder Response -> m Response
            go = \case
              Binary.Fail _ _ _ -> liftIO $ fail "Response parse failure"
              Binary.Done remainder _ x -> x <$ writeIORef remainderReference remainder
              Binary.Partial f -> do
                bs <- liftIO $ Network.appRead socket
                go $ f $ mfilter (not . ByteString.null) (Just bs)
         in do
              input <- readIORef remainderReference
              go $ Binary.pushChunk (Binary.runGetIncremental (Binary.get @Response)) input

  forever do
    message <- readMessage
    handler <- atomically $ TQueue.readTQueue responseHandlerQueue
    handler message

class (MonadIO m) => Connected m where
  sendMessage :: Request -> ResponseHandler a -> m a

instance Connected Client where
  sendMessage request (responseHandler :: ResponseHandler a) =
    Client \Connection {requestQueue, responseHandlerQueue} -> do
      (responseHandlerFunction :: Response -> IO (), result :: IO a) <- case responseHandler of
        IgnoreResponse -> pure (\_ -> pure (), pure ())
        AwaitResponse f -> do
          resultReference <- newEmptyMVar
          let send = putMVar resultReference
              receive :: IO a
              receive = do
                result <- takeMVar resultReference
                f result & maybe (fail "unexpected message type") pure
          pure (send, receive)
      atomically do
        TQueue.writeTQueue requestQueue request
        TQueue.writeTQueue responseHandlerQueue responseHandlerFunction
      result

instance (Connected m) => Connected (MaybeT m) where
  sendMessage message responseHandler =
    MaybeT (Just <$> sendMessage message responseHandler)
