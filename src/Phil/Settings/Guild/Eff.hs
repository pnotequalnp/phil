{-# LANGUAGE TemplateHaskell #-}

module Phil.Settings.Guild.Eff where

import Calamity (Guild, HasID, User)
import Data.IntMap (IntMap)
import Data.Text.Lazy (Text)
import Phil.Database (Reminder)
import Polysemy (makeSem)

data GuildSettings m a where
  GetGuildPrefix :: HasID Guild guild => guild -> GuildSettings m Text
  SetGuildPrefix :: HasID Guild guild => guild -> Text -> GuildSettings m Text
  UnsetGuildPrefix :: HasID Guild guild => guild -> GuildSettings m Text
  AddUserReminder :: (HasID Guild guild, HasID User user) => guild -> user -> Int -> Text -> GuildSettings m (Int, Reminder)
  GetUserReminders :: (HasID Guild guild, HasID User user) => guild -> user -> GuildSettings m (IntMap Reminder)
  RemoveUserReminder :: (HasID Guild guild, HasID User user) => guild -> user -> Int -> GuildSettings m ()

makeSem ''GuildSettings
