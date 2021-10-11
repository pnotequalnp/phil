module Phil.Events.MessageCreate where

import Calamity (BotC, EventType (MessageCreateEvt), Message, ReactConstraints)
import Data.Foldable (traverse_)
import Phil.Events.Reminders (sendReminders)
import Phil.Settings.Guild.Eff (GuildSettings)
import Polysemy (Members, Sem)
import Polysemy.Random (Random)

messageCreate ::
  ( BotC r,
    ReactConstraints 'MessageCreateEvt,
    Members '[GuildSettings, Random] r
  ) =>
  Message ->
  Sem r ()
messageCreate msg = traverse_ ($ msg) [sendReminders]
