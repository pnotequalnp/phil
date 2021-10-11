module Phil.Events where

import Calamity (BotC, EventType (..), react)
import Phil.Events.MessageCreate (messageCreate)
import Phil.Settings.Guild.Eff (GuildSettings)
import Polysemy (Members, Sem)
import Polysemy.Random (Random)

registerEvents :: (BotC r, Members '[GuildSettings, Random] r) => Sem r ()
registerEvents = do
  _ <- react @'MessageCreateEvt messageCreate
  pure ()
