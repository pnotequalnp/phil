module Phil.Commands.Reminders where

import Calamity (User, reply)
import Calamity.Commands (command)
import Control.Lens
import Control.Monad
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Phil.Commands.Util
import Phil.Database (Reminder)
import Phil.Settings.Guild.Eff (GuildSettings, addUserReminder, getUserReminders, removeUserReminder)
import Polysemy.Error (note, runError)

reminders :: CommandC '[GuildSettings] r => CommandM r ()
reminders = do
  _ <- command @'[User, Int, Text] "remind" \ctx user freq msg -> do
    res <- runError do
      guild <- ctx ^. #guild & note "not in guild"
      member <- ctx ^. #member & note "not a guild member"
      isGuildAdmin member >>= insist "permission denied"
      guard (freq > 0 && freq <= 100) & note "frequency must be between 1 and 100"
      addUserReminder guild user freq msg
    let response = either ("Error: " <>) (const "They will remember. :)") res
    _ <- reply @Text (ctx ^. #message) response
    pure ()

  _ <- command @'[User] "reminders" \ctx user -> do
    res <- runError do
      guild <- ctx ^. #guild & note "not in guild"
      getUserReminders guild user
    let response = either ("Error: " <>) renderReminders res
    _ <- reply @Text (ctx ^. #message) response
    pure ()

  _ <- command @'[User, Int] "forget" \ctx user reminder -> do
    res <- runError do
      guild <- ctx ^. #guild & note "not in guild"
      member <- ctx ^. #member & note "not a guild member"
      isGuildAdmin member >>= insist "permission denied"
      removeUserReminder guild user reminder
    let response = either ("Error: " <>) (const "Forgotten!") res
    _ <- reply @Text (ctx ^. #message) response
    pure ()

  pure ()

renderReminders :: IntMap Reminder -> Text
renderReminders rs
  | null rs = "No reminders"
  | otherwise = T.intercalate "\n" . toList . IM.mapWithKey singleReminder $ rs
  where
    singleReminder key reminder =
      T.pack (show key)
        <> " ("
        <> T.pack (show $ reminder ^. #reminderFrequency)
        <> "%): "
        <> reminder ^. #reminderMessage
