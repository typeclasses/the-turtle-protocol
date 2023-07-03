import Control.Concurrent (threadDelay)
import Env qualified
import Ki.Unlifted qualified as Ki
import Linear.Metric (norm)
import Relude hiding (break)
import TurtleProtocol.Action
import TurtleProtocol.Client qualified as Turtle
import TurtleProtocol.Socket qualified as TurtleProtocol

main :: IO ()
main = do
  runtimeDirectory <- liftIO $ Env.parse id $ Env.var Env.str "XDG_RUNTIME_DIR" mempty
  socketPath <- TurtleProtocol.getDefaultSocketPath (Just runtimeDirectory)
  Turtle.runUnix socketPath $ Ki.scoped \threadScope -> do
    _ <- Ki.fork threadScope $ star "Red" 0 "red"
    _ <- Ki.fork threadScope $ star "Blue" 180 "blue"
    atomically $ Ki.awaitAll threadScope

star :: (Turtle.Connected m) => Text -> Double -> Text -> m ()
star t startAngle color = do
  setSpeed t "fast"

  setPenColor t color
  beginFill t

  left t startAngle

  whileTrue do
    forward t 200
    p <- getPosition t
    when (norm p < 1) break
    left t 170
    pauseSeconds (2 / 10)

  endFill t

  putTextLn $ "Done: " <> t

break :: (Applicative m) => MaybeT m a
break = MaybeT (pure Nothing)

whileTrue :: (Monad m) => MaybeT m a1 -> m ()
whileTrue a = void $ runMaybeT $ forever a

pauseSeconds :: (MonadIO m) => Rational -> m ()
pauseSeconds x = liftIO $ threadDelay $ round $ x * 1_000_000
