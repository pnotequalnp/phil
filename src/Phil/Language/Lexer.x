{
{-# OPTIONS_GHC -w #-}

module Phil.Language.Lexer (
  LexError (..),
  Token (..),
  scanTokens
) where

import Data.Text (Text)
import Data.Text qualified as T
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import Prettyprinter (Pretty (..), dquotes)
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  $eol                          { \s -> TokenNewline }
  $white+                       ;
  "--".*                        ;
  let                           { \s -> TokenLet }
  True                          { \s -> TokenBool True }
  False                         { \s -> TokenBool False }
  in                            { \s -> TokenIn }
  ":="                          { \s -> TokenDef }
  $digit+                       { \s -> TokenInt (read s) }
  \\                            { \s -> TokenLambda }
  "->"                          { \s -> TokenArrow }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSymbol (T.pack s) }

{
data Token
  = TokenInt Integer
  | TokenFloat Double
  | TokenString Text
  | TokenBool Bool
  | TokenSymbol Text
  | TokenLambda
  | TokenArrow
  | TokenLet
  | TokenDef
  | TokenIn
  | TokenLParen
  | TokenRParen
  | TokenNewline
  deriving stock (Eq, Show)

instance Pretty Token where
  pretty = \case
    TokenInt x -> pretty x
    TokenFloat x -> pretty x
    TokenString s -> dquotes $ pretty s
    TokenBool b -> pretty b
    TokenSymbol s -> pretty s
    TokenLambda -> "\\"
    TokenArrow -> "->"
    TokenLet -> "let"
    TokenDef -> ":="
    TokenIn -> "in"
    TokenLParen -> "("
    TokenRParen -> ")"
    TokenNewline -> "\\n"

data LexError
  deriving stock (Show)

instance Pretty LexError where
  pretty = \case

scanTokens :: Member (Error LexError) r => Text -> Sem r [Token]
scanTokens = pure . alexScanTokens . T.unpack
}
