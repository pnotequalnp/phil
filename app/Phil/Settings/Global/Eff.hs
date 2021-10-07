{-# LANGUAGE TemplateHaskell #-}

module Phil.Settings.Global.Eff where

import Calamity (HasID, User)
import Data.Text.Lazy (Text)
import Polysemy (makeSem)

data GlobalSettings m a where
  GetPrefix :: GlobalSettings m Text
  SetPrefix :: Text -> GlobalSettings m Text
  ResetAll :: GlobalSettings m ()
  IsGlobalAdmin :: HasID User user => user -> GlobalSettings m Bool

makeSem ''GlobalSettings
