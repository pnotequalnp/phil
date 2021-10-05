module Phil.Server where

import Calamity (Guild, Snowflake (..))
import Control.Lens
import Data.Text.Lazy (Text)
import Data.Word (Word64)
import Phil.Settings (Settings, getPrefix, setPrefix)
import Polysemy (Members, Sem)
import Polysemy.AtomicState (AtomicState, atomicGet, atomicState)
import Polysemy.Error (Error)
import Servant
import Data.Tuple (swap)

type PrefixAPI =
  Capture "guild_id" Word64 :> Get '[JSON] (Maybe Text)
    :<|> Capture "guild_id" Word64 :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe Text)
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Text
    :<|> Get '[JSON] Text

-- TODO: authentication
prefixServer :: Members '[AtomicState Settings, Error ServerError] r => ServerT PrefixAPI (Sem r)
prefixServer = getGuildPrefix :<|> setGuildPrefix :<|> setDefaultPrefix :<|> getDefaultPrefix
  where
    getGuildPrefix (Snowflake @Guild -> gid) = getPrefix gid
    setGuildPrefix (Snowflake @Guild -> gid) = setPrefix gid . Just
    getDefaultPrefix = view #defaultPrefix <$> atomicGet
    setDefaultPrefix newP = atomicState $ swap . (#defaultPrefix <<.~ newP)
