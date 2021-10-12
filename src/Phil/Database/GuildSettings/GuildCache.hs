module Phil.Database.GuildSettings.GuildCache where

import Calamity (Guild, Snowflake, User)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Phil.Database (Reminder)

data GuildCache = GuildCache
  { prefix :: Map (Snowflake Guild) Text,
    reminders :: Map (Snowflake Guild) (Map (Snowflake User) (IntMap Reminder))
  }
  deriving stock (Generic)
