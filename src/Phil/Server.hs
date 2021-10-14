module Phil.Server where

import Calamity (Guild, Snowflake (..))
import Data.Text.Lazy (Text)
import Data.Word (Word64)
import Phil.Settings.Global.Eff (GlobalSettings, getPrefix, setPrefix)
import Phil.Settings.Guild.Eff (GuildSettings, getGuildPrefix, setGuildPrefix)
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import Servant

type PrefixAPI =
  Capture "guild_id" Word64 :> Get '[JSON] Text
    :<|> Capture "guild_id" Word64 :> ReqBody '[JSON] Text :> Post '[JSON] ()
    :<|> Get '[JSON] Text
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Text

prefixServer :: Members '[GuildSettings, GlobalSettings, Error ServerError] r => ServerT PrefixAPI (Sem r)
prefixServer = getGuild :<|> setGuild :<|> getDefault :<|> setDefault
  where
    getGuild = getGuildPrefix . Snowflake @Guild
    setGuild = setGuildPrefix . Snowflake @Guild
    getDefault = getPrefix
    setDefault = setPrefix
