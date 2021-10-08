module Phil.Commands.Util where

import Calamity qualified as C
import Calamity.Commands (DSLState)
import Calamity.Commands.Context (FullContext)
import CalamityCommands (ConstructContext, ParsePrefix)
import Control.Lens
import Data.Flags ((.>=.))
import Data.Vector.Unboxing qualified as V
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error, throw)

type CommandC cs r =
  ( C.BotC r,
    Members '[ParsePrefix C.Message, ConstructContext C.Message FullContext IO ()] r,
    Members cs r
  )

type CommandM r = Sem (DSLState FullContext r)

insist :: Member (Error e) r => e -> Bool -> Sem r ()
insist _ True = pure ()
insist err False = throw err

isGuildAdmin :: C.BotC r => C.Member -> Sem r Bool
isGuildAdmin member =
  V.foldM (\z -> fmap (z ||) . roleHasAdmin (member ^. #guildID)) False $ member ^. #roles

roleHasAdmin :: C.BotC r => C.Snowflake C.Guild -> C.Snowflake C.Role -> Sem r Bool
roleHasAdmin guild roleflake =
  C.upgrade (guild, roleflake) <&> \case
    Nothing -> False
    Just role -> role ^. #permissions .>=. C.administrator
