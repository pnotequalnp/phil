module Phil.Commands where

import Calamity (Message, SetupEff)
import Calamity.Cache.Eff (CacheEff)
import Calamity.Commands (addCommands, group, helpCommand)
import Calamity.Commands.Context (FullContext)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import CalamityCommands (ConstructContext, ParsePrefix)
import Control.Monad
import Phil.Commands.GlobalSettings (globalSettingsCommands)
import Phil.Commands.Prefix (prefixCommands)
import Phil.Settings.Global.Eff (GlobalSettings)
import Phil.Settings.Guild.Eff (GuildSettings)
import Polysemy (Embed, Final, Members, Sem)

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
  group "prefix" prefixCommands
  group "globalSettings" globalSettingsCommands
