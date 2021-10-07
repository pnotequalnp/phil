{-# LANGUAGE TemplateHaskell #-}

module Phil.Settings.Guild.Eff where

import Calamity (Guild, HasID)
import Data.Text.Lazy (Text)
import Polysemy (makeSem)

data GuildSettings m a where
  GetGuildPrefix :: HasID Guild guild => guild -> GuildSettings m Text
  SetGuildPrefix :: HasID Guild guild => guild -> Text -> GuildSettings m Text
  UnsetGuildPrefix :: HasID Guild guild => guild -> GuildSettings m Text

makeSem ''GuildSettings
