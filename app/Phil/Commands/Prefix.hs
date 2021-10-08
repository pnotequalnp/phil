module Phil.Commands.Prefix where

import Calamity (reply)
import Calamity.Commands (command)
import Control.Lens
import Control.Monad
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Phil.Commands.Util
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix)
import Phil.Settings.Guild.Eff (GuildSettings, getGuildPrefix, setGuildPrefix, unsetGuildPrefix)
import Polysemy.Error (note, runError)

prefixCommands :: CommandC '[GuildSettings, GlobalSettings] r => CommandM r ()
prefixCommands = do
  _ <- command @'[] "get" \ctx -> do
    prefix <- ctx ^. #guild & maybe getPrefix getGuildPrefix
    _ <- reply (ctx ^. #message) prefix
    pure ()

  _ <- command @'[Text] "set" \ctx newPrefix -> do
    res <- runError do
      guild <- ctx ^. #guild & note "not in guild"
      guard (T.length newPrefix < 3) & note "too long"
      member <- ctx ^. #member & note "not a guild member"
      isGuildAdmin member >>= insist "permission denied"
      setGuildPrefix guild newPrefix
    let msg = either ("Error: " <>) (((ctx ^. #prefix) <> " -> ") <>) res
    _ <- reply (ctx ^. #message) msg
    pure ()

  _ <- command @'[] "unset" \ctx -> do
    res <- runError do
      guild <- ctx ^. #guild & note "not in guild"
      member <- ctx ^. #member & note "not a guild member"
      isGuildAdmin member >>= insist "permission denied"
      unsetGuildPrefix guild
    let m = either ("Error: " <>) (((ctx ^. #prefix) <> " -> ") <>) res
    _ <- reply (ctx ^. #message) m
    pure ()

  pure ()
