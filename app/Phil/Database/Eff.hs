{-# LANGUAGE TemplateHaskell #-}

module Phil.Database.Eff where

import Database.Persist.Sql (SqlPersistT, Update, entityVal, getBy, upsert)
import Phil.Database (GlobalSettings, globalSettings)
import Polysemy (makeSem)

type Transaction = SqlPersistT IO

data Persist m a where
  Transact :: Transaction a -> Persist m a

makeSem ''Persist

getGlobalSettings :: Transaction (Maybe GlobalSettings)
getGlobalSettings = fmap entityVal <$> getBy globalSettings

updateGlobalSettings :: GlobalSettings -> [Update GlobalSettings] -> Transaction GlobalSettings
updateGlobalSettings = fmap entityVal .: upsert
  where
    (.:) = (.) . (.)

setGlobalSettings :: GlobalSettings -> Transaction ()
setGlobalSettings _ = pure () -- # TODO
