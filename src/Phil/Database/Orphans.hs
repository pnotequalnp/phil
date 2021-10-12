{-# OPTIONS_GHC -Wno-orphans #-}

module Phil.Database.Orphans where

import Calamity (Snowflake (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))

instance PersistField (Snowflake a) where
  toPersistValue = toPersistValue . fromSnowflake
  fromPersistValue = fmap Snowflake . fromPersistValue

instance PersistFieldSql (Snowflake a) where
  sqlType _ = sqlType $ Proxy @Word64
