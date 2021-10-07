module Phil.Commands where

import Calamity (BotC, Message, SetupEff, reply, upgrade)
import Calamity qualified as C
import Calamity.Cache.Eff (CacheEff)
import Calamity.Commands (addCommands, command, group, helpCommand)
import Calamity.Commands.Context (FullContext)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import CalamityCommands (ConstructContext, ParsePrefix)
import Control.Lens
import Control.Monad
import Data.Flags ((.>=.))
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Vector.Unboxing qualified as V
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix, isGlobalAdmin, setPrefix)
import Phil.Settings.Guild.Eff (GuildSettings, getGuildPrefix, setGuildPrefix, unsetGuildPrefix)
import Polysemy (Embed, Final, Member, Members, Sem)
import Polysemy.Error (Error, note, runError, throw)

registerCommands ::
  Members
    '[ GuildSettings,
       GlobalSettings,
       ParsePrefix Message,
       ConstructContext Message FullContext IO (),
       Final IO,
       Embed IO,
       CacheEff,
       LogEff,
       MetricEff
     ]
    r =>
  Sem (SetupEff r) ()
registerCommands = void $ addCommands do
  _ <- helpCommand

  _ <- group "prefix" do
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

  _ <- group "globalSettings" do
    _ <- group "prefix" do
      _ <- command @'[Text] "set" \ctx newPrefix -> do
        res <- runError do
          isGlobalAdmin (ctx ^. #user) >>= insist "permission denied"
          setPrefix newPrefix
        let m = either ("Error: " <>) ("Old: " <>) res
        _ <- reply (ctx ^. #message) m
        pure ()

      _ <- command @'[] "get" \ctx -> void $ getPrefix >>= reply (ctx ^. #message)

      pure ()

    pure ()

  pure ()
  where
    insist :: Member (Error e) r => e -> Bool -> Sem r ()
    insist _ True = pure ()
    insist err False = throw err

    isGuildAdmin :: BotC r => C.Member -> Sem r Bool
    isGuildAdmin member =
      V.foldM (\z -> fmap (z ||) . roleHasAdmin (member ^. #guildID)) False $ member ^. #roles

    roleHasAdmin :: BotC r => C.Snowflake C.Guild -> C.Snowflake C.Role -> Sem r Bool
    roleHasAdmin guild roleflake =
      upgrade (guild, roleflake) <&> \case
        Nothing -> False
        Just role -> role ^. #permissions .>=. C.administrator
