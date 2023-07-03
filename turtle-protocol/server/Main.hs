import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict qualified as Map
import Data.Streaming.Network qualified as Network
import Env qualified
import Graphics.X11.Turtle (Turtle)
import Graphics.X11.Turtle qualified as Turtle
import Ki.Unlifted qualified as Ki
import Linear.V2 (V2 (..))
import Relude
import TurtleProtocol.Message (Request, Response)
import TurtleProtocol.Message qualified as TM
import TurtleProtocol.Socket qualified as TurtleProtocol

main :: IO ()
main = Ki.scoped \threadScope -> do
  runtimeDirectory <- liftIO $ Env.parse id $ Env.var Env.str "XDG_RUNTIME_DIR" mempty
  socketPath <- TurtleProtocol.getDefaultSocketPath (Just runtimeDirectory)
  field <- Turtle.openField
  _ <-
    Ki.fork threadScope $
      Network.runUnixServer (Network.serverSettingsUnix socketPath) $
        serve field
  Turtle.waitField field
  pure ()

serve :: (MonadIO m, Network.HasReadWrite p) => Turtle.Field -> p -> m ()
serve field socket = do
  remainderReference <- newIORef ByteString.empty

  let readMessage :: (MonadIO m) => m (Maybe Request)
      readMessage =
        let go = \case
              Binary.Fail _ _ _ -> pure Nothing
              Binary.Done remainder _ x -> do
                writeIORef remainderReference remainder
                pure (Just x)
              Binary.Partial f -> do
                bs <- liftIO $ Network.appRead socket
                go $ f $ mfilter (not . ByteString.null) (Just bs)
         in do
              input <- readIORef remainderReference
              go $ Binary.pushChunk (Binary.runGetIncremental (Binary.get @Request)) input

      sendMessage :: (MonadIO m) => Response -> m ()
      sendMessage message =
        traverse_ (liftIO . Network.appWrite socket) $
          LazyByteString.toChunks (Binary.runPut (Binary.put @Response message))

  turtlesByName :: IORef (Map Text Turtle) <- newIORef Map.empty

  let getOrCreateTurtle :: (MonadIO m) => Text -> m Turtle
      getOrCreateTurtle name =
        (readIORef turtlesByName <&> Map.lookup name) >>= \case
          Just turtle -> pure turtle
          Nothing -> do
            turtle <- liftIO $ Turtle.newTurtle field
            modifyIORef' turtlesByName (Map.insert name turtle)
            pure turtle

      lookupTurtle :: (MonadIO m) => Text -> m (Maybe Turtle)
      lookupTurtle name = readIORef turtlesByName <&> Map.lookup name

  void $
    runMaybeT $
      forever $
        readMessage >>= maybe (MaybeT (pure Nothing)) \case
          TM.SetSpeed {TM.turtle, TM.speed} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.speed turtle' (toString speed)
            sendMessage TM.Nil
          TM.SetPenColor {TM.turtle, TM.color} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.pencolor turtle' (toString color)
            sendMessage TM.Nil
          TM.SetPenSize {TM.turtle, TM.size} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.pensize turtle' size
            sendMessage TM.Nil
          TM.BeginFill {TM.turtle} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.beginfill turtle'
            sendMessage TM.Nil
          TM.EndFill {TM.turtle} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.endfill turtle'
            sendMessage TM.Nil
          TM.Forward {TM.turtle, TM.distance} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.forward turtle' distance
            sendMessage TM.Nil
          TM.Left {TM.turtle, TM.degrees} -> do
            turtle' <- getOrCreateTurtle turtle
            liftIO $ Turtle.left turtle' degrees
            sendMessage TM.Nil
          TM.GetPosition {TM.turtle} -> do
            lookupTurtle turtle >>= \case
              Nothing -> sendMessage TM.Nil
              Just turtle' -> do
                (x, y) <- liftIO $ Turtle.position turtle'
                sendMessage TM.PositionIs {TM.position = V2 x y}
