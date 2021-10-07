module Phil.Settings where

import Calamity (Guild, HasID (..), Message, Snowflake)
import CalamityCommands (ParsePrefix (..))
import Control.Lens
import Data.Either (fromRight)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, interpret)
import Polysemy.AtomicState (AtomicState, atomicGet, atomicState)
import Polysemy.Error (note, runError)

data Settings = Settings
  { prefix :: Map (Snowflake Guild) Text,
    defaultPrefix :: Text
  }
  deriving stock (Show, Generic)

getPrefix :: (HasID Guild g, Member (AtomicState Settings) r) => g -> Sem r (Maybe Text)
getPrefix g = atomicGet <&> view (#prefix . at (getID g))

setPrefix :: (HasID Guild g, Member (AtomicState Settings) r) => g -> Maybe Text -> Sem r (Maybe Text)
setPrefix g p = atomicState $ swap . (#prefix . at (getID g) <<.~ p)

customGuildPrefix :: Member (AtomicState Settings) r => Sem (ParsePrefix Message ': r) a -> Sem r a
customGuildPrefix = interpret \case
  ParsePrefix msg -> do
    defP <- view #defaultPrefix <$> atomicGet
    p <-
      fromRight defP <$> runError do
        g <- msg ^. #guildID & note ()
        getPrefix g >>= note ()
    pure $ (p,) <$> T.stripPrefix p (msg ^. #content)
