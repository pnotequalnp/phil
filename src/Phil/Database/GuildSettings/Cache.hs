module Phil.Database.GuildSettings.Cache where

import Calamity (Snowflake, getID)
import Calamity qualified as C
import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.IORef (newIORef)
import Data.IntMap (IntMap)
import Data.Map qualified as M
import Data.Text.Lazy (Text)
import Phil.Database (Reminder)
import Phil.Database.GuildSettings.GuildCache (GuildCache (GuildCache))
import Phil.Settings.Guild.Eff (GuildSettings (..), addUserReminder, getGuildPrefix, getUserReminders, removeUserReminder, setGuildPrefix, unsetGuildPrefix)
import Polysemy (Embed, Members, Sem, embed, reinterpret2)
import Polysemy.AtomicState (AtomicState, atomicGets, atomicModify, runAtomicStateIORef)

-- | Assumes the underlying storage does not get changed by other actors.
aggressivelyCacheGuildSettings ::
  Members '[Embed IO] r => Sem (GuildSettings ': r) a -> Sem (AtomicState GuildCache ': GuildSettings ': r) a
aggressivelyCacheGuildSettings = reinterpret2 \case
  GetGuildPrefix (getID -> guild) ->
    atomicGets (^. #prefix . at guild) >>= maybe (getGuildPrefix guild >>= setCachePrefix guild) pure
  SetGuildPrefix (getID -> guild) prefix ->
    void $ setGuildPrefix guild prefix *> setCachePrefix guild prefix
  UnsetGuildPrefix (getID -> guild) ->
    unsetGuildPrefix guild >>= setCachePrefix guild
  AddUserReminder (getID -> guild) (getID -> user) freq msg -> do
    (key, reminder) <- addUserReminder guild user freq msg
    atomicModify $ #reminders . at guild . non mempty . at user . non mempty . at key ?~ reminder
    pure (key, reminder)
  GetUserReminders (getID -> guild) (getID -> user) ->
    atomicGets (^. #reminders . ix guild . at user)
      >>= maybe (getUserReminders guild user >>= setCacheReminders guild user) pure
  RemoveUserReminder (getID -> guild) (getID -> user) reminder -> do
    atomicModify $ #reminders . at guild . non mempty . at user . non mempty . at reminder .~ Nothing
    removeUserReminder guild user reminder
  where
    setCacheReminders ::
      Members '[AtomicState GuildCache] r =>
      Snowflake C.Guild ->
      Snowflake C.User ->
      IntMap Reminder ->
      Sem r (IntMap Reminder)
    setCacheReminders guild user reminders = do
      atomicModify $ #reminders . at guild . non mempty . at user ?~ reminders
      pure reminders

    setCachePrefix :: Members '[AtomicState GuildCache] r => Snowflake C.Guild -> Text -> Sem r Text
    setCachePrefix guild prefix = do
      atomicModify $ #prefix . at guild ?~ prefix
      pure prefix

aggressivelyCacheGuildSettingsInMemory ::
  Members '[Embed IO] r => Sem (GuildSettings ': r) a -> Sem (GuildSettings ': r) a
aggressivelyCacheGuildSettingsInMemory x = do
  cache <- embed . newIORef $ GuildCache M.empty M.empty
  runAtomicStateIORef cache $ aggressivelyCacheGuildSettings x
