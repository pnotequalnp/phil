module Phil.Database.GuildSettings where

import Calamity (HasID (..))
import Calamity qualified as C
import Control.Lens
import Data.Generics.Labels ()
import Data.Text.Lazy (Text)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist (entityKey, (=.))
import Database.Persist qualified as DB
import Phil.Database
  ( EntityField (..),
    Guild (Guild),
    Key (..),
    Reminder (Reminder),
    Unique (SnowflakeGuild, SnowflakeUser),
    User (User),
  )
import Phil.Database.Eff (Persist, transact)
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix)
import Phil.Settings.Guild.Eff (GuildSettings (..))
import Polysemy (Members, Sem, interpret)
import qualified Data.IntMap as IM

persistGuildSettings :: Members '[Persist, GlobalSettings] r => Sem (GuildSettings ': r) a -> Sem r a
persistGuildSettings = interpret \case
  GetGuildPrefix (getID -> guild) ->
    transact (DB.getBy $ SnowflakeGuild guild) >>= \case
      Just (view #guildPrefix . DB.entityVal -> prefix) -> pure prefix
      Nothing -> do
        prefix <- getPrefix
        _key <- transact . DB.insert $ Guild guild prefix
        pure prefix
  SetGuildPrefix guild prefix -> setGuild guild prefix
  UnsetGuildPrefix guild -> getPrefix >>= setGuild guild
  AddUserReminder (getID -> guild) (getID -> user) freq msg -> do
    guildKey <-
      (transact . DB.getBy) (SnowflakeGuild guild) >>= \case
        Just entity -> pure . entityKey $ entity
        Nothing -> getPrefix >>= transact . DB.insert . Guild guild
    userKey <-
      (transact . DB.getBy) (SnowflakeUser user) >>= \case
        Just entity -> pure . entityKey $ entity
        Nothing -> transact . DB.insert $ User user False
    let reminder = Reminder userKey guildKey msg freq
    ReminderKey key <- transact $ DB.insert reminder
    pure (fromEnum key, reminder)
  GetUserReminders (getID -> guild) (getID -> user) -> do
    reminderEntities <- transact . E.select $ do
      (r E.:& g E.:& u) <- E.from $ E.table @Reminder
        `E.innerJoin` E.table @Guild `E.on` (\(r E.:& g) -> r E.^. ReminderGuild E.==. g E.^. GuildId)
        `E.innerJoin` E.table @User `E.on` (\(r E.:& _ E.:& u) -> r E.^. ReminderUser E.==. u E.^. UserId)
      E.where_ $ g E.^. GuildSnowflake E.==. E.val guild
      E.where_ $ u E.^. UserSnowflake E.==. E.val user
      pure r
    let reminders = reminderEntities <&> \(DB.Entity (ReminderKey key) reminder) -> (fromEnum key, reminder)
    pure $ IM.fromList reminders
  RemoveUserReminder _ _ (ReminderKey . toEnum -> key) ->
    transact $ DB.delete key
  where
    setGuild :: (Members '[Persist, GlobalSettings] r, HasID C.Guild guild) => guild -> Text -> Sem r Text
    setGuild (getID -> guild) prefix =
      transact $
        DB.upsert (Guild guild prefix) [GuildPrefix =. prefix]
          <&> view #guildPrefix . DB.entityVal
