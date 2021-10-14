module Phil.Database.GlobalSettings where

import Calamity (getID)
import Control.Lens
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Database.Persist ((=.))
import Database.Persist qualified as DB
import Phil.Database (EntityField (GlobalSettingsPrefix), GlobalSettings, userGlobalAdmin, userflake)
import Phil.Database.Eff (Persist, getGlobalSettings, setGlobalSettings, transact, updateGlobalSettings)
import Phil.Settings.Global.Eff qualified as Eff
import Polysemy (Members, Sem, interpret)

persistGlobalSettings ::
  Members '[Persist] r =>
  GlobalSettings ->
  Sem (Eff.GlobalSettings ': r) a ->
  Sem r a
persistGlobalSettings initialSettings = interpret \case
  Eff.GetPrefix -> do
    settings <- fromMaybe initialSettings <$> transact getGlobalSettings
    pure $ settings ^. #globalSettingsPrefix
  Eff.SetPrefix prefix -> do
    oldSettings <- transact $ updateGlobalSettings initialSettings [GlobalSettingsPrefix =. prefix]
    pure $ oldSettings ^. #globalSettingsPrefix
  Eff.ResetAll -> transact $ setGlobalSettings initialSettings
  Eff.IsGlobalAdmin (view userflake . getID -> user) ->
    transact $ DB.get user <&> maybe False userGlobalAdmin
