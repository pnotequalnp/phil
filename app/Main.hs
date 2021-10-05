module Main where

import Calamity (Token (..), runBotIO)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop (runMetricsNoop)
import Data.Default (def)
import Data.Generics.Labels ()
import Data.IORef (newIORef)
import Data.Map qualified as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Di qualified
import DiPolysemy (info, runDiToIO)
import Phil.Commands (registerCommands)
import Phil.Server (PrefixAPI, prefixServer)
import Phil.Settings (Settings (Settings), customGuildPrefix)
import Polysemy (embedToFinal, runFinal)
import Polysemy.Async (async)
import Polysemy.AtomicState (runAtomicStateIORef)
import Servant.Polysemy.Server (runWarpServer)
import System.Environment (getEnv)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  tok <- BotToken . T.pack <$> getEnv "PHIL_TOKEN"
  port <- read @Int <$> getEnv "PHIL_PORT"
  settings <- newIORef $ Settings M.empty "!"
  err <- Di.new $ \di ->
    runFinal
      . embedToFinal @IO
      . runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . runAtomicStateIORef settings
      . useFullContext
      . customGuildPrefix
      $ runBotIO tok def do
        info @Text "Starting initialization"

        _ <- async do
          info @Text "Starting web server"
          runWarpServer @PrefixAPI port True prefixServer
          info @Text "Web server stopped"

        registerCommands

        info @Text "Initialization complete"
  case err of
    Nothing -> pure ()
    Just e -> hPrint stderr e
