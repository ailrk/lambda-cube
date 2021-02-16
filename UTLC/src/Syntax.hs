{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Syntax where

import           AST
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Maybe
import           Data.Sequence             as S
import           Debug.Trace
import           Text.Parsec               as P
import           Text.Parsec.Language      as P
import           Text.Parsec.String        as P
import qualified Text.Parsec.Token         as Tok


newtype UTLCPState = PState (Seq String)

lexer :: Tok.TokenParser UTLCPState
lexer = Tok.makeTokenParser style
  where
    rops   = ["\\"]
    rnames = []
    style = emptyDef { Tok.reservedOpNames = rops
                     , Tok.reservedNames   = rnames
                     , Tok.commentLine     = "--"
                     }

type UTLCParser = Parsec String UTLCPState

reserved :: String -> UTLCParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> UTLCParser ()
reservedOp = Tok.reservedOp lexer

identifier :: UTLCParser String
identifier = Tok.identifier lexer

parens :: UTLCParser a -> UTLCParser a
parens = Tok.parens lexer

contents :: UTLCParser a -> UTLCParser a
contents p = Tok.whiteSpace lexer >> p >>= (\r -> eof >> return r)

natural :: UTLCParser Integer
natural = Tok.natural lexer

variable :: UTLCParser Expr
variable = do
  var <- identifier
  idx <- getDeburijnIndex 0 var

  return (Var idx)
  where
    getDeburijnIndex n var = getState >>= \case
      (PState S.Empty)    -> fail "Unbounded variable"
      (PState (xs :|> x)) ->
        if var == x
           then return n
           else do setState (PState xs); getDeburijnIndex (n + 1) var

number :: UTLCParser Expr
number = natural >>= return . Lit . VInt . fromIntegral

lambda :: UTLCParser Expr
lambda = do
  reservedOp "\\"
  arg <- identifier
  reservedOp "."
  pushParam arg
  body <- expr
  return $ Lam body
  where
    pushParam arg = getState >>=
      \(PState vars) -> when (arg /= "_") $ setState (PState (vars |> arg))

term :: UTLCParser Expr
term = parens expr <|> lambda <|> variable <|> number

expr :: UTLCParser Expr
expr = foldl1 App <$> many1 term

parseUTLC :: String -> Either ParseError Expr
parseUTLC = runP (contents expr) (PState S.empty) ""
