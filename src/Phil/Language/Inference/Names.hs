{-# LANGUAGE TemplateHaskell #-}

module Phil.Language.Inference.Names where

import Control.Monad
import Data.Function
import Data.Text (Text, pack)
import Phil.Language (Name)
import Polysemy (Sem, makeSem, reinterpret)
import Polysemy.State (evalState, get, modify)

data Names m a where
  FreshName :: Names m Name

makeSem ''Names

sequentialNames :: Sem (Names : r) a -> Sem r a
sequentialNames x =
  evalState @Int 0 $
    x & reinterpret \case
      FreshName -> do
        name <- (names !!) <$> get
        modify (+ 1)
        pure name

names :: [Text]
names = [1 ..] >>= fmap pack . flip replicateM ['a' .. 'z']
