module Phil.Language.Inference where

import Control.Lens
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Phil.Language.Inference.Names
import Phil.Language.Syntax
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error, note, throw)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.Reader (Reader, asks, local, runReader)
import Polysemy.State (execState, get, put)
import Prettyprinter (Pretty (..), squotes, (<+>))

newtype Env = TypeEnv (Map Name PType)
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (Ixed)

type instance Index Env = Name

type instance IxValue Env = PType

instance At Env where
  at x f (TypeEnv m) = TypeEnv <$> at x f m

data TypeError
  = UnboundVariable Name
  | UnificationFailure PType PType
  | InfiniteType TVar PType
  | Redefinition Name
  deriving stock (Show)

instance Pretty TypeError where
  pretty = \case
    UnboundVariable name -> "Unbound variable" <+> squotes (pretty name)
    UnificationFailure a b ->
      "Cannot match type" <+> squotes (pretty a) <+> "with type" <+> squotes (pretty b)
    InfiniteType var a ->
      "Cannot construct infinite type" <+> squotes (pretty var <+> "~" <+> pretty a)
    Redefinition name -> "Multiple definitions of" <+> squotes (pretty name)

data PConstraint = PType :~: PType

infix 3 :~:

newtype Subst = Subst (Map TVar PType)
  deriving stock (Show)
  deriving newtype (Monoid)

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ (apply (Subst s1) <$> s2) <> s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TVar

instance Substitutable PType where
  apply s@(Subst m) = \case
    TCon x -> TCon x
    TVar x -> M.findWithDefault (TVar x) x m
    x :>> y -> apply s x :>> apply s y
    TForall vars t -> TForall vars $ apply s' t
      where
        s' = Subst $ foldr M.delete m vars

  ftv = \case
    TCon _ -> mempty
    TVar x -> S.singleton x
    x :>> y -> ftv x <> ftv y
    TForall vars t -> ftv t `S.difference` S.fromList vars

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ apply s <$> env
  ftv (TypeEnv env) = foldMap ftv env

instance Substitutable PConstraint where
  apply s (x :~: y) = apply s x :~: apply s y
  ftv (x :~: y) = ftv x <> ftv y

type Module = Map Name (PExpr, PType)

-- Type inference

inferExpr :: Member (Error TypeError) r => Env -> PExpr -> Sem r PType
inferExpr env expr = do
  (constraints, t) <- runInference env expr
  subst <- solve mempty constraints
  pure $ apply subst t

inferModule :: Member (Error TypeError) r => [(Name, PExpr)] -> Sem r Module
inferModule decls = do
  (constraints, types) <-
    decls
      & sequentialNames . runOutputList . execState mempty . traverse \(name, expr) -> do
        env <- get
        env ^. at name & \case
          Nothing -> pure ()
          Just _ -> throw $ Redefinition name
        t_expr <- runReader env $ infer expr
        put $ env & at name ?~ t_expr
        pure ()
  subst <- solve mempty constraints
  let types' = apply subst types
  pure . M.fromList $ decls <&> \(name, expr) ->
    (name, (expr, fromMaybe (error "inference died") $ types' ^. at name))

-- Constraint generation

runInference :: Member (Error TypeError) r => Env -> PExpr -> Sem r ([PConstraint], PType)
runInference env = runOutputList . sequentialNames . runReader env . infer

infer :: Members '[Reader Env, Names, Output PConstraint, Error TypeError] r => PExpr -> Sem r PType
infer = \case
  EVar x -> asks (^. at x) >>= note (UnboundVariable x) >>= instantiate
  EApp f x -> do
    t_f <- infer f
    t_x <- infer x
    t_fx <- TVar . TV <$> freshName
    output $ t_f :~: t_x :>> t_fx
    pure t_fx
  ELam x body -> do
    t_x <- TVar . TV <$> freshName
    t_body <- local (at x ?~ t_x) $ infer body
    pure $ t_x :>> t_body
  ELet x e body -> do
    t_x <- infer e >>= generalize
    local (at x ?~ t_x) $ infer body
  ELit lit -> pure . TCon . TC $ case lit of
    LInt _ -> "Int"
    LFloat _ -> "Float"
    LString _ -> "String"
    LBool _ -> "Bool"

generalize :: Member (Reader Env) r => PType -> Sem r PType
generalize = \case
  TForall vars t -> pure $ TForall vars t
  t -> do
    env <- asks ftv
    let vars = S.toList $ ftv t `S.difference` env
    pure $ TForall vars t

instantiate :: Member Names r => PType -> Sem r PType
instantiate = \case
  TForall vars a -> do
    vars' <- traverse (fmap (TVar . TV) . const freshName) vars
    let subst = Subst . M.fromList $ zip vars vars'
    pure $ apply subst a
  a -> pure a

-- Constraint solving

solve :: Member (Error TypeError) r => Subst -> [PConstraint] -> Sem r Subst
solve subst = \case
  [] -> pure subst
  ((t1 :~: t2) : constraints) -> do
    subst' <- unify t1 t2
    solve (subst' <> subst) $ apply subst' <$> constraints

unify :: Member (Error TypeError) r => PType -> PType -> Sem r Subst
unify t1 t2 | t1 == t2 = pure mempty
unify (TVar x) t = x `bind` t
unify t (TVar x) = x `bind` t
unify (x1 :>> x2) (y1 :>> y2) = do
  s <- unify x1 y1
  s' <- unify (apply s x2) (apply s y2)
  pure $ s <> s'
unify t1 t2 = throw $ UnificationFailure t1 t2

bind :: Member (Error TypeError) r => TVar -> PType -> Sem r Subst
bind x t
  | t == TVar x = pure mempty
  | x `S.member` ftv t = throw $ InfiniteType x t
  | otherwise = pure . Subst $ M.singleton x t
