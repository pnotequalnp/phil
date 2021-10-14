module Phil.Database.GuildSettings where

import Calamity (HasID (..))
import Calamity qualified as C
import Control.Lens
import Data.Generics.Labels ()
import Data.IntMap qualified as IM
import Data.Text.Lazy (Text)
import Database.Persist ((=.), (==.))
import Database.Persist qualified as DB
import Phil.Database
  ( EntityField (..),
    Guild (Guild),
    Key (..),
    Reminder (Reminder),
    guildflake,
    userflake,
  )
import Phil.Database.Eff (Persist, transact)
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix)
import Phil.Settings.Guild.Eff (GuildSettings (..))
import Polysemy (Members, Sem, interpret)

persistGuildSettings :: Members '[Persist, GlobalSettings] r => Sem (GuildSettings ': r) a -> Sem r a
persistGuildSettings = interpret \case
  GetGuildPrefix (view guildflake . getID -> guild) ->
    transact (DB.get guild) >>= \case
      Just (view #guildPrefix -> prefix) -> pure prefix
      Nothing -> do
        prefix <- getPrefix
        _key <- transact . DB.insert $ Guild prefix
        pure prefix
  SetGuildPrefix guild prefix -> setGuild guild prefix
  UnsetGuildPrefix guild -> do
    prefix <- getPrefix
    setGuild guild prefix
    pure prefix
  AddUserReminder (view guildflake . getID -> guild) (view userflake . getID -> user) freq msg -> do
    let reminder = Reminder user guild msg freq
    ReminderKey key <- transact $ DB.insert reminder
    pure (fromEnum key, reminder)
  GetUserReminders (view guildflake . getID -> guild) (view userflake . getID -> user) -> do
    entities <- transact (DB.selectList [ReminderUser ==. user, ReminderGuild ==. guild] [])
    let reminders = entities <&> \(DB.Entity (ReminderKey key) reminder) -> (fromEnum key, reminder)
    pure $ IM.fromList reminders
  RemoveUserReminder _ _ (ReminderKey . fromIntegral -> key) ->
    transact $ DB.delete key
  where
    setGuild :: (Members '[Persist, GlobalSettings] r, HasID C.Guild guild) => guild -> Text -> Sem r ()
    setGuild (view guildflake . getID -> guild) prefix =
      transact $ DB.get guild >>= \case
        Just g | g ^. #guildPrefix /= prefix -> DB.update guild [GuildPrefix =. prefix]
        Just _ -> pure ()
        Nothing -> DB.insertKey guild $ Guild prefix
