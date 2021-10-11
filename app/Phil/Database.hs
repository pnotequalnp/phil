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
    Unique (SnowflakeUser, SnowflakeGuild),
    globalSettings,
    migrateAll,
  )
where

import Calamity qualified as C
import Data.Default (Default (..))
import Data.Text.Lazy (Text)
import Database.Persist (EntityField, Key, Unique)
import Database.Persist.TH
import GHC.Generics (Generic)
import Phil.Database.Orphans ()

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Guild
    snowflake (C.Snowflake C.Guild)
    prefix Text
    SnowflakeGuild snowflake
    deriving Show Generic

GlobalSettings
    sentinel Int
    prefix Text
    Sentinel sentinel
    deriving Show Generic

User
    snowflake (C.Snowflake C.User)
    globalAdmin Bool
    SnowflakeUser snowflake
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
