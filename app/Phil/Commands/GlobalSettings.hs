module Phil.Commands.GlobalSettings where

import Calamity (reply)
import Calamity.Commands (command, group)
import Control.Lens
import Control.Monad
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Phil.Commands.Util
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix, isGlobalAdmin, setPrefix)
import Phil.Settings.Guild.Eff (GuildSettings)
import Polysemy.Error (note, runError)

globalSettingsCommands :: CommandC '[GuildSettings, GlobalSettings] r => CommandM r ()
globalSettingsCommands = do
  group "prefix" globalPrefixCommands

globalPrefixCommands :: CommandC '[GuildSettings, GlobalSettings] r => CommandM r ()
globalPrefixCommands = do
  _ <- command @'[Text] "set" \ctx newPrefix -> do
    res <- runError do
      isGlobalAdmin (ctx ^. #user) >>= insist "permission denied"
      guard (T.length newPrefix < 3) & note "too long"
      setPrefix newPrefix
    let m = either ("Error: " <>) ("Old: " <>) res
    _ <- reply (ctx ^. #message) m
    pure ()

  _ <- command @'[] "get" \ctx -> void $ getPrefix >>= reply (ctx ^. #message)

  pure ()
