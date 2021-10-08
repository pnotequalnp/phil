module Phil.Commands.GlobalSettings where

import Calamity (reply)
import Calamity.Commands (command, group)
import Control.Lens
import Data.Functor
import Data.Text.Lazy (Text)
import Phil.Commands.Util
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix, isGlobalAdmin, setPrefix)
import Phil.Settings.Guild.Eff (GuildSettings)
import Polysemy.Error (runError)

globalSettingsCommands :: CommandC '[GuildSettings, GlobalSettings] r => CommandM r ()
globalSettingsCommands = do
  group "prefix" globalPrefixCommands

globalPrefixCommands :: CommandC '[GuildSettings, GlobalSettings] r => CommandM r ()
globalPrefixCommands = do
  _ <- command @'[Text] "set" \ctx newPrefix -> do
    res <- runError do
      isGlobalAdmin (ctx ^. #user) >>= insist "permission denied"
      setPrefix newPrefix
    let m = either ("Error: " <>) ("Old: " <>) res
    _ <- reply (ctx ^. #message) m
    pure ()

  _ <- command @'[] "get" \ctx -> void $ getPrefix >>= reply (ctx ^. #message)

  pure ()
