module Main where

import Calamity (Snowflake (..), Token (..), runBotIO)
import Calamity qualified as C
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad
import Data.Default (def)
import Data.Flags (allFlags)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as L
import Database.Persist ((=.))
import Database.Persist qualified as DB
import Database.Persist.Sql qualified as DB
import Di qualified
import DiPolysemy (info, runDiToIO)
import Phil.Commands (registerCommands)
import Phil.Database (EntityField (UserGlobalAdmin), Unique (SnowflakeUser), User (..), migrateAll)
import Phil.Database.Eff (transact)
import Phil.Database.GlobalSettings (persistGlobalSettings)
import Phil.Database.GuildSettings (persistGuildSettings)
import Phil.Database.GuildSettings.Cache (aggressivelyCacheGuildSettingsInMemory)
import Phil.Database.Sqlite (sqlitePersistPooled)
import Phil.Events (registerEvents)
import Phil.Server (PrefixAPI, prefixServer)
import Phil.Settings.Guild.Prefix (customGuildPrefix)
import Polysemy (embedToFinal, runFinal)
import Polysemy.Async (async)
import Polysemy.Random.IO (randomIO)
import Servant.Polysemy.Server (runWarpServer)
import System.Environment (getEnv)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  tok <- BotToken . L.pack <$> getEnv "PHIL_TOKEN"
  port <- read <$> getEnv "PHIL_PORT"
  dbFile <- T.pack <$> getEnv "PHIL_DATABASE"
  dbConnections <- read <$> getEnv "PHIL_DB_CONNECTIONS"
  adminId <- Snowflake @C.User . read <$> getEnv "PHIL_ADMIN"

  err <- Di.new $ \di ->
    runFinal
      . embedToFinal
      . randomIO
      . runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      . sqlitePersistPooled dbFile dbConnections
      . persistGlobalSettings def
      . persistGuildSettings
      . aggressivelyCacheGuildSettingsInMemory
      . customGuildPrefix
      $ runBotIO tok allFlags do
        info @Text "Starting initialization"

        info @Text "Preparing database"
        transact do
          DB.runMigration migrateAll
          DB.getBy (SnowflakeUser adminId) >>= \case
            Nothing -> void . DB.insert $ User {userSnowflake = adminId, userGlobalAdmin = True}
            Just DB.Entity {DB.entityVal = User {userGlobalAdmin = True}} -> pure ()
            Just (DB.entityKey -> adminKey) -> DB.update adminKey [UserGlobalAdmin =. True]

        info @Text "Starting web server"
        _serverThread <- async do
          runWarpServer @PrefixAPI port True prefixServer
          info @Text "Web server stopped"

        info @Text "Registering commands"
        registerCommands

        info @Text "Registering event handlers"
        registerEvents

        info @Text "Initialization complete"
  case err of
    Nothing -> pure ()
    Just e -> hPrint stderr e
