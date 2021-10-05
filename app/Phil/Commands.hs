module Phil.Commands where

import Calamity (Message, SetupEff, tell, upgrade)
import Calamity.Cache.Eff (CacheEff)
import Calamity.Commands (addCommands, helpCommand, group, command)
import Calamity.Commands.Context (FullContext)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import CalamityCommands (ConstructContext, ParsePrefix)
import Control.Monad
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Phil.Settings (Settings, getPrefix, setPrefix)
import Polysemy (Embed, Final, Members, Sem)
import Polysemy.AtomicState (AtomicState, atomicGet)
import Polysemy.Error (runError, note)
import Control.Lens
import Data.Maybe (fromMaybe)

registerCommands ::
  Members
    '[ AtomicState Settings,
       ParsePrefix Message,
       ConstructContext Message FullContext IO (),
       Final IO,
       Embed IO,
       CacheEff,
       LogEff,
       MetricEff
     ]
    r =>
  Sem (SetupEff r) ()
registerCommands = void $ addCommands do
  _ <- helpCommand

  _ <- group "prefix" do
    _ <- command @'[] "get" \ctx -> do
      res <- runError do
        g <- ctx ^. #guild & note "Not in guild"
        getPrefix g
      defP <- view #defaultPrefix <$> atomicGet @Settings
      let m = either ("Error: " <>) (fromMaybe $ "Default (" <> defP <> ")") res
      _ <- tell (ctx ^. #message) m
      pure ()

    _ <- command @'[Text] "set" \ctx newP -> do
      res <- runError do
        g <- ctx ^. #guild & note "not in guild"
        guard (T.length newP < 3) & note "too long"
        _a <- upgrade (ctx ^. #message . #author) >>= note "internal error (failed to retrieve author)"
        -- TODO: verify author has valid privileges
        setPrefix g (Just newP)
      defP <- view #defaultPrefix <$> atomicGet @Settings
      let m = either ("Error: " <>) ((<> " -> " <> newP) . fromMaybe defP) res
      _ <- tell (ctx ^. #message) m
      pure ()

    _ <- command @'[] "unset" \ctx -> do
      res <- runError do
        g <- ctx ^. #guild & note "not in guild"
        _a <- upgrade (ctx ^. #message . #author) >>= note "internal error (failed to retrieve author)"
        -- TODO: verify author has valid privileges
        setPrefix g Nothing >>= note "prefix not set"
      defP <- view #defaultPrefix <$> atomicGet @Settings
      let m = either ("Error: " <>) (<> " -> Default (" <> defP <> ")") res
      _ <- tell (ctx ^. #message) m
      pure ()

    pure ()

  pure ()
