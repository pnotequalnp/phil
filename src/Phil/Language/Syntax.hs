module Phil.Language.Syntax where

import Data.Text (Text)
import Prettyprinter (Pretty (..), dquotes, hsep, (<+>))

type Name = Text

data PLit
  = LInt Integer
  | LFloat Double
  | LString Text
  | LBool Bool
  deriving stock (Show)

instance Pretty PLit where
  pretty = \case
    LInt x -> pretty x
    LFloat x -> pretty x
    LString s -> dquotes . pretty $ s
    LBool b -> pretty b

data PExpr
  = EVar Name
  | EApp PExpr PExpr
  | ELam Name PExpr
  | ELet Name PExpr PExpr
  | ELit PLit
  deriving stock (Show)

instance Pretty PExpr where
  pretty = \case
    EVar name -> pretty name
    EApp x y -> pretty x <+> pretty y
    ELam name body -> "\\" <> pretty name <+> "->" <+> pretty body
    ELet name e body -> "let" <+> pretty name <+> ":=" <+> pretty e <+> "in" <+> pretty body
    ELit x -> pretty x

newtype TVar = TV Name
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty TVar where
  pretty (TV name) = pretty name

newtype TyCon = TC Name
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty TyCon where
  pretty (TC name) = pretty name

data PType
  = TVar TVar
  | TCon TyCon
  | PType :>> PType
  | TForall [TVar] PType
  deriving stock (Eq, Ord, Show)

infixr 5 :>>

instance Pretty PType where
  pretty = \case
    TVar a -> pretty a
    TCon a -> pretty a
    a :>> b -> pretty a <+> "->" <+> pretty b
    TForall [] a -> pretty a
    TForall vars a -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty a
