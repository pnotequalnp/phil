module Polysemy.Random.IO where

import Polysemy (Embed, Member, Sem, embed, interpret)
import Polysemy.Random qualified as PR
import System.Random qualified as SR

-- | Safe to use after `asyncToIOFinal`
randomIO :: Member (Embed IO) r => Sem (PR.Random ': r) a -> Sem r a
randomIO = interpret \case
  PR.Random -> embed SR.randomIO
  PR.RandomR range -> embed $ SR.randomRIO range
