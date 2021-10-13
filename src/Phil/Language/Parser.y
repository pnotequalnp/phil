{
module Phil.Language.Parser (ParseError (..), parseExpr, parseModule) where

import Phil.Language.Lexer
import Phil.Language.Syntax
import Polysemy (Member, Sem, run)
import Polysemy.Error (Error, runError, throw)
import Prettyprinter (Pretty (..), squotes, (<+>))
}

%name expr Expr
%name parseMod
%tokentype { Token }
%monad { Sem '[Error ParseError] } { (>>=) } { pure }
%error { parseError }

%token
    let   { TokenLet }
    true  { TokenBool True }
    false { TokenBool False }
    in    { TokenIn }
    NUM   { TokenInt $$ }
    VAR   { TokenSymbol $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    ':='  { TokenDef }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    '\n'  { TokenNewline }

%%

Module : Decl                      { [$1] }
       | Module '\n' Decl          { $3 : $1 }
       | Decl '\n'                 { [$1] }

Decl : VAR ':=' Expr               { ($1, $3) }

Expr : let VAR ':=' Expr in Expr   { ELet $2 $4 $6 }
     | '\\' VAR '->' Expr          { ELam $2 $4 }
     | Form                        { $1 }

Form : Fact                        { $1 }

Fact : Fact Atom                   { EApp $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { ELit (LInt $1) }
     | VAR                         { EVar $1 }
     | true                        { ELit (LBool True) }
     | false                       { ELit (LBool False) }

{

data ParseError
  = Unexpected Token
  | EOF
  deriving stock (Show)

instance Pretty ParseError where
  pretty = \case
    Unexpected token -> "Unexpected token" <+> squotes (pretty token)
    EOF -> "Unexpected end of input"

parseError :: Member (Error ParseError) r => [Token] -> Sem r a
parseError (t : _) = throw $ Unexpected t
parseError [] = throw EOF

parseExpr :: Member (Error ParseError) r => [Token] -> Sem r PExpr
parseExpr = either throw pure . run . runError . expr

parseModule :: Member (Error ParseError) r => [Token] -> Sem r [(Name, PExpr)]
parseModule = either throw pure . run . runError . parseMod

}
