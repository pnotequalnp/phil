module Phil.Language where

import Control.Monad
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Phil.Language.Inference (Module, TypeError, inferExpr, inferModule)
import Phil.Language.Lexer (LexError, scanTokens)
import Phil.Language.Parser (ParseError)
import Phil.Language.Parser qualified as Parser
import Phil.Language.Syntax (PExpr, PType)
import Polysemy (Members, Sem)
import Polysemy.Error (Error, mapError)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

data CompilerError
  = TypeError TypeError
  | ParseError ParseError
  | LexError LexError
  deriving stock (Show)

parseExpr :: Members '[Error CompilerError] r => Text -> Sem r (PExpr, PType)
parseExpr source = do
  tokens <- mapError LexError $ scanTokens source
  expr <- mapError ParseError $ Parser.parseExpr tokens
  t_expr <- mapError TypeError $ inferExpr mempty expr
  pure (expr, t_expr)

parseModule :: Members '[Error CompilerError] r => Text -> Sem r Module
parseModule =
  mapError LexError . scanTokens
    >=> mapError ParseError . Parser.parseModule
    >=> mapError TypeError . inferModule

browsePretty :: Module -> Text
browsePretty = T.intercalate "\n" . fmap go . M.toList . fmap snd
  where
    go (name, t) = renderStrict . layoutPretty defaultLayoutOptions $ pretty name <+> ":" <+> pretty t

prettyShow :: PExpr -> PType -> Text
prettyShow expr t_expr = renderStrict . layoutPretty defaultLayoutOptions $ pretty expr <+> ":" <+> pretty t_expr
