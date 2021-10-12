module Phil.Language where

import Data.Text (Text)

type Name = Text

data PLit
  = LInt Integer
  | LFloat Double
  | LString Text
  | LBool Bool
  deriving stock (Show)

data PExpr
  = EVar Name
  | EApp PExpr PExpr
  | ELam Name PExpr
  | ELet Name PExpr PExpr
  | ELit PLit
  deriving stock (Show)

newtype TVar = TV Name
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype TyCon = TC Name
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data PType
  = TVar TVar
  | TCon TyCon
  | PType :>> PType
  | TForall [TVar] PType
  deriving stock (Eq, Ord, Show)

infixr 5 :>>
