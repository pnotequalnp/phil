{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Phil.Database
  ( EntityField (..),
    GlobalSettings (globalSettingsPrefix),
    Guild (..),
    User (..),
    Unique (SnowflakeUser, SnowflakeGuild),
    globalSettings,
    migrateAll,
  )
where

import Data.Default (Default (..))
import Data.Text.Lazy (Text)
import Data.Word (Word64)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Guild
    snowflake Word64
    prefix Text
    SnowflakeGuild snowflake
    deriving Show Generic

GlobalSettings
    sentinel Int
    prefix Text
    Sentinel sentinel
    deriving Show Generic

User
    snowflake Word64
    globalAdmin Bool
    SnowflakeUser snowflake
|]

instance Default GlobalSettings where
  def = GlobalSettings 0 "!"

globalSettings :: Unique GlobalSettings
globalSettings = Sentinel 0
