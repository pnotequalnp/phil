module Main where

import Calamity (Token (..), runBotIO)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad
import Data.Default (def)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as L
import Database.Persist qualified as DB
import Database.Persist.Sql qualified as DB
import Di qualified
import DiPolysemy (info, runDiToIO)
import Phil.Commands (registerCommands)
import Phil.Database (Unique (SnowflakeUser), User (..), migrateAll)
import Phil.Database.Eff (transact)
import Phil.Database.GlobalSettings (persistGlobalSettings)
import Phil.Database.GuildSettings (persistGuildSettings, aggressivelyCacheInMemoryGuildSettings)
import Phil.Database.Sqlite (sqlitePersistPooled)
import Phil.Server (PrefixAPI, prefixServer)
import Phil.Settings.Guild.Prefix (customGuildPrefix)
import Polysemy (embedToFinal, runFinal)
import Polysemy.Async (async)
import Servant.Polysemy.Server (runWarpServer)
import System.Environment (getEnv)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  tok <- BotToken . L.pack <$> getEnv "PHIL_TOKEN"
  port <- read <$> getEnv "PHIL_PORT"
  dbFile <- T.pack <$> getEnv "PHIL_DATABASE"
  dbConnections <- read <$> getEnv "PHIL_DB_CONNECTIONS"
  adminId <- read <$> getEnv "PHIL_ADMIN"

  err <- Di.new $ \di ->
    runFinal
      . embedToFinal
      . runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      . sqlitePersistPooled dbFile dbConnections
      . persistGlobalSettings def
      . persistGuildSettings
      . aggressivelyCacheInMemoryGuildSettings
      . customGuildPrefix
      $ runBotIO tok def do
        info @Text "Starting initialization"

        transact do
          DB.runMigration migrateAll
          DB.getBy (SnowflakeUser adminId) >>= \case
            Nothing -> void . DB.insert $ User {userSnowflake = adminId, userGlobalAdmin = True}
            Just DB.Entity {DB.entityVal = User {userGlobalAdmin = True}} -> pure ()
            Just (DB.entityKey -> adminKey) -> DB.update adminKey []

        _serverThread <- async do
          info @Text "Starting web server"
          runWarpServer @PrefixAPI port True prefixServer
          info @Text "Web server stopped"

        registerCommands

        info @Text "Initialization complete"
  case err of
    Nothing -> pure ()
    Just e -> hPrint stderr e
