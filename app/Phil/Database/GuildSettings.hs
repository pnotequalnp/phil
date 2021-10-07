module Phil.Database.GuildSettings where

import Calamity (Snowflake, fromSnowflake, getID, HasID)
import Calamity qualified as C
import Control.Lens
import Data.Generics.Labels ()
import Data.IORef (newIORef)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text.Lazy (Text)
import Database.Persist ((=.))
import Database.Persist qualified as DB
import Phil.Database (EntityField (GuildPrefix), Guild (Guild), Unique (SnowflakeGuild))
import Phil.Database.Eff (Persist, transact)
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix)
import Phil.Settings.Guild.Eff (GuildSettings (..), getGuildPrefix, setGuildPrefix, unsetGuildPrefix)
import Polysemy (Embed, Members, Sem, embed, interpret, reinterpret2)
import Polysemy.AtomicState (AtomicState, atomicGets, atomicModify, runAtomicStateIORef)

persistGuildSettings ::
  Members '[Persist, GlobalSettings] r =>
  Sem (GuildSettings ': r) a ->
  Sem r a
persistGuildSettings = interpret \case
  GetGuildPrefix (fromSnowflake @C.Guild . getID -> guild) ->
    transact (DB.getBy $ SnowflakeGuild guild) >>= \case
      Just (view #guildPrefix . DB.entityVal -> prefix) -> pure prefix
      Nothing -> do
        prefix <- getPrefix
        _key <- transact . DB.insert $ Guild guild prefix
        pure prefix
  SetGuildPrefix guild prefix -> setGuild guild prefix
  UnsetGuildPrefix guild -> getPrefix >>= setGuild guild
  where
    setGuild :: (Members '[Persist, GlobalSettings] r, HasID C.Guild guild) => guild -> Text -> Sem r Text
    setGuild (fromSnowflake @C.Guild . getID -> guild) prefix =
      transact $
        DB.upsert (Guild guild prefix) [GuildPrefix =. prefix]
          <&> view #guildPrefix . DB.entityVal

type Cache = Map (Snowflake C.Guild) Text

-- | Assumes the underlying storage does not get changed by other actors.
aggressivelyCachedGuilSettings ::
  Members '[Embed IO] r => Sem (GuildSettings ': r) a -> Sem (AtomicState Cache ': GuildSettings ': r) a
aggressivelyCachedGuilSettings = reinterpret2 \case
  GetGuildPrefix (getID -> guild) -> atomicGets (view $ at guild) >>= maybe (getGuildPrefix guild) pure
  SetGuildPrefix (getID -> guild) prefix -> do
    prefix' <- setGuildPrefix guild prefix
    atomicModify $ at guild ?~ prefix'
    pure prefix'
  UnsetGuildPrefix (getID -> guild) -> do
    prefix <- unsetGuildPrefix guild
    atomicModify $ at guild ?~ prefix
    pure prefix

aggressivelyCacheInMemoryGuildSettings ::
  Members '[Embed IO] r => Sem (GuildSettings ': r) a -> Sem (GuildSettings ': r) a
aggressivelyCacheInMemoryGuildSettings x = do
  cache <- embed $ newIORef M.empty
  runAtomicStateIORef cache $ aggressivelyCachedGuilSettings x
