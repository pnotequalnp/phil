module Phil.Database.Sqlite where

import Control.Lens
import Control.Monad.Logger (NoLoggingT (..))
import Data.Text (Text)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Phil.Database.Eff (Persist (..))
import Polysemy (Embed, Member, Sem, embed, interpret)

sqlitePersistPooled :: forall r a. Member (Embed IO) r => Text -> Int -> Sem (Persist ': r) a -> Sem r a
sqlitePersistPooled conn n x = do
  pool <- embed . runNoLoggingT $ createSqlitePool conn n
  x & interpret \case
    Transact transaction -> embed $ runSqlPool transaction pool
