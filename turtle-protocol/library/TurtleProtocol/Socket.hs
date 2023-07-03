module TurtleProtocol.Socket where

import Relude
import System.Environment qualified as Env
import System.FilePath ((</>))

getDefaultSocketPath :: (MonadIO m) => Maybe FilePath -> m FilePath
getDefaultSocketPath runtimeDirectoryMaybe = do
  runtimeDirectory <- maybe (liftIO $ Env.getEnv "XDG_RUNTIME_DIR") pure runtimeDirectoryMaybe
  pure $ runtimeDirectory </> "turtle-server.socket"
