{-# LANGUAGE AllowAmbiguousTypes #-}

module Phil.Language.Interpreter where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Phil.Language.Syntax
import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.Reader (Reader, ask, asks, local, runReader)
import Prettyprinter (Pretty (..))

data Value
  = VInt Integer
  | VBool Bool
  | VFloat Double
  | VString Text
  | VClosure Name PExpr Env
  deriving stock (Show)

data PrimType = PrimInt | PrimBool | PrimFloat | PrimString

class PrimVal v where
  type Val v
  primVal :: Value -> Maybe (Val v)

instance PrimVal 'PrimInt where
  type Val 'PrimInt = Integer
  primVal = \case
    VInt x -> Just x
    _ -> Nothing

instance PrimVal 'PrimBool where
  type Val 'PrimBool = Bool
  primVal = \case
    VBool b -> Just b
    _ -> Nothing

instance Pretty Value where
  pretty = \case
    VInt x -> pretty x
    VBool b -> pretty b
    VFloat x -> pretty x
    VString s -> pretty s
    VClosure {} -> "function"

newtype Env = Env (Map Name Value)
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (Ixed)

type instance Index Env = Name

type instance IxValue Env = Value

instance At Env where
  at x f (Env m) = Env <$> at x f m

eval :: Members '[Reader Env, Fail] r => PExpr -> Sem r Value
eval = \case
  EVar name -> asks (^. at name) >>= maybe (fail "unbound variable") pure
  ELet name expr body -> do
    val <- eval expr
    local (at name ?~ val) $ eval body
  ELam name body -> VClosure name body <$> ask
  EApp f x ->
    eval f >>= \case
      VClosure name expr env -> do
        arg <- eval x
        local (const $ env & at name ?~ arg) $ eval expr
      _ -> fail "not a function"
  ELit l -> pure case l of
    LInt x -> VInt x
    LFloat x -> VFloat x
    LString s -> VString s
    LBool b -> VBool b

runEval :: PExpr -> Either String Value
runEval = run . runFail . runReader mempty . eval
