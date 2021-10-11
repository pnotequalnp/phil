module Phil.Events.Reminders where

import Calamity (BotC, Message, reply)
import Control.Lens
import Control.Monad
import Data.Foldable (for_)
import Phil.Settings.Guild.Eff (GuildSettings, getUserReminders)
import Polysemy (Members, Sem)
import Polysemy.Random (Random, randomR)

sendReminders :: (BotC r, Members '[GuildSettings, Random] r) => Message -> Sem r ()
sendReminders msg = do
  reminders <-
    msg ^. #guildID & \case
      Nothing -> pure mempty
      Just guild -> getUserReminders guild $ msg ^. #author
  for_ reminders \reminder -> do
    rand <- randomR (0, 99)
    when (rand < reminder ^. #reminderFrequency) . void $
      reply msg (reminder ^. #reminderMessage)
