{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Phil.Database
  ( EntityField (..),
    GlobalSettings (globalSettingsPrefix),
    Guild (..),
    Key (..),
    Reminder (..),
    User (..),
    globalSettings,
    guildflake,
    migrateAll,
    userflake,
  )
where

import Calamity qualified as C
import Data.Default (Default (..))
import Data.Text.Lazy (Text)
import Database.Persist (EntityField, Key, Unique)
import Database.Persist.TH
import GHC.Generics (Generic)
import Phil.Database.Orphans ()
import Control.Lens

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Guild
    prefix Text
    deriving Show Generic

GlobalSettings
    sentinel Int
    prefix Text
    Sentinel sentinel
    deriving Show Generic

User
    globalAdmin Bool
    deriving Show Generic

Reminder
    user UserId
    guild GuildId
    message Text
    frequency Int
    deriving Show Generic Ord Eq
|]

instance Default GlobalSettings where
  def = GlobalSettings 0 "!"

globalSettings :: Unique GlobalSettings
globalSettings = Sentinel 0

guildflake :: Iso' (C.Snowflake C.Guild) (Key Guild)
guildflake = iso (GuildKey . fromIntegral . C.fromSnowflake)
  (C.Snowflake . fromIntegral . \(GuildKey key) -> key)

userflake :: Iso' (C.Snowflake C.User) (Key User)
userflake = iso (UserKey . fromIntegral . C.fromSnowflake)
  (C.Snowflake . fromIntegral . \(UserKey key) -> key)
