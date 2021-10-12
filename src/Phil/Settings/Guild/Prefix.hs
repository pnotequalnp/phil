module Phil.Settings.Guild.Prefix where

import Calamity (Message)
import CalamityCommands (ParsePrefix (..))
import Control.Lens
import Data.Text.Lazy qualified as T
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix)
import Phil.Settings.Guild.Eff (GuildSettings, getGuildPrefix)
import Polysemy (Members, Sem, interpret)

customGuildPrefix :: Members '[GlobalSettings, GuildSettings] r => Sem (ParsePrefix Message ': r) a -> Sem r a
customGuildPrefix = interpret \case
  ParsePrefix msg -> do
    p <- msg ^. #guildID & maybe getPrefix getGuildPrefix
    pure $ (p,) <$> T.stripPrefix p (msg ^. #content)
