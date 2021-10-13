module Phil.Language.Repl where

import Control.Category ((>>>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Phil.Language (CompilerError (..), parseExpr)
import Phil.Language.Interpreter (runEval)
import Polysemy (Embed, Members, Sem, embed, run)
import Polysemy.Error (runError)
import Polysemy.Input (Input, input, runInputSem)
import Polysemy.Output (Output, output, runOutputSem)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

data Action
  = NoOp
  | Quit
  | Evaluate Text
  | Infer Text
  | AST Text

parseLine :: Text -> Action
parseLine l = fromMaybe (Evaluate l) $ case T.breakOn " " l of
  (":q", _) -> Just Quit
  (":t", e) -> Just $ Infer e
  (":ast", e) -> Just $ AST e
  _ -> Nothing

dispatch :: Action -> Maybe (Doc ann)
dispatch = \case
  NoOp -> Just mempty
  Quit -> Nothing
  Evaluate e -> Just $ eval e
  Infer e -> Just $ infer e
  AST e -> Just $ ast e
  where
    load f =
      parseExpr >>> runError >>> run >>> \case
        Right x -> f x
        Left (TypeError err) -> "Type Error:" <+> pretty err
        Left (ParseError err) -> "Parse Error:" <+> pretty err
        Left (LexError err) -> "Lex Error:" <+> pretty err
    infer = load $ pretty . snd
    ast = load $ pretty . fst
    eval = load $ fst >>> runEval >>> \case
      Right v -> pretty v
      Left err -> "Impossible:" <+> pretty err

repl :: Members '[Input Text, Output (Doc ann)] r => Sem r ()
repl = do
  output "Phil++ > "
  line <- input
  case (dispatch . parseLine) line of
    Nothing -> pure ()
    Just doc -> output doc *> repl

consoleRepl :: Members '[Embed IO] r => Sem r ()
consoleRepl = runInputSem (embed T.getLine) . runOutputSem (embed . T.putStrLn . render) $ repl
  where
    render = renderStrict . layoutPretty defaultLayoutOptions
