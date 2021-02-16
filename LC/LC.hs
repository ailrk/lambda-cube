{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

{-@ Just lambda calculus
    Use church encodings to represent values.
    snippet1:
      let -- integer n is encoded as applying f by n times.
          0 = \f x -> x
          1 = \f x -> f x
          2 = \f x -> f (f x)
          -- fᵐ⁺ⁿ(x) = fᵐ(fⁿ(x))
          plus = \m n f x -> m f (n f x)
          -- succ (n) = n + 1 β≡ plus n 1
          succ = \n f x -> f (n f x)
          -- fᵐˣⁿ(x) = (fⁿ)ᵐ(x)
          mult = \m n f x -> m (n f) x
          expt = \m n -> n m

       in mul (exp 2 3) 2
 @-}

import           Data.Maybe           as M
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Tok


main = undefined

test = readFile "LC.lam"

-- ast --
type Name = String
data Expr = App Expr Expr
          | Lam String Expr
          | Var String
          | Lit Lit
#ifndef Pretty
          deriving Show
#endif

data Lit = LInt Integer
         | LBool Bool
#ifndef Pretty
          deriving Show
#endif

type Env = [(Name, Expr)]


#ifdef Pretty
instance Show Lit where
  show (LInt n)      = show n
  show (LBool True)  = "#t"
  show (LBool False) = "#f"

instance Show Expr where
  show (Lam s t) = "\\" ++ s ++ showB t where
    showB (Lam x y) = " " ++ x ++ showB y
    showB expr      = "." ++ show expr
  show (Var s) = s
  show (App x y) = showL x ++ showR y where
    showL (Lam _ _) = "(" ++ show x ++ ")"
    showL _         = show x
    showR (Var s) = " " ++ s
    showR _       = "(" ++ show y ++ ")"
  show (Lit n) = show n
#endif

-- parser --
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef { Tok.commentLine = "--" }

identifier :: Parser String
identifier = notFollowedBy (string "let") *> Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

ws :: Parser ()
ws = Tok.whiteSpace lexer

var :: Parser Expr
var = Var <$> identifier

symbol :: String -> Parser String
symbol = Tok.symbol lexer

-- support let expression
-- real lambda calculus only recurse with Y combinator. let allows you to do self reference.
-- top level parser parse the whole lange as an environment.
lang :: Parser [Either (Name, Expr) Expr]
lang = many line <?> "neither top level binding nor expression"
  where
    line = (Left <$> try bind) <|> (Right <$> expr)
    bind = (,) <$> ((symbol "let") *> identifier <* ws <* (symbol "=")) <*> expr

-- lambda calculus expr, watch out for left recursion
expr :: Parser Expr
expr = (try app) <|> lam

-- extended syntax. \a b -> n => \a -> \b -> n
lam :: Parser Expr
lam = do
  symbol "\\"
  namelist <- many1 identifier
  symbol "->"
  e <- term
  case namelist of
    [n] -> return $ Lam n e
    xs  -> return $ mklam xs e
  where
    mklam [x] body    = Lam x body
    mklam (x:xs) body = Lam x (mklam xs body)

app :: Parser Expr
app = do
  apps <- many1 term
  return $ foldl1 App apps  -- lil trick with foldl1.when xs is [n] return term.

-- avoid left recursion
term' :: Parser Expr
term' = ((try lit) <|> (try var) <|> (between (char '(') (char ')') expr)) <* ws

-- context sensitive. It's for parsing \x-> x (a b), if a var is followed by a app, group them.
-- some traceback ticks.
term :: Parser Expr
term = do
  v <- term'
  try $ (do v' <- try term'
            return (App v v'))
            <|> return v

lit :: Parser Expr
lit = Lit <$> (try boolLit <|> intLit)
  where
    boolLit = do
      v <- (try (string "#t")) <|> string "#f"
      return . LBool $ case v of
        "#t" -> True
        "#f" -> False
        _    -> error $ "failed to parse literal: " ++ v
    intLit = LInt <$> integer

-- evaluator
-- lambda calculus has this renaming thing needs to take care of.
-- in application, when free variable is bound, all occurence

eval :: Env -> Expr -> Expr
eval env (Var n) = case lookup n env of
                     Just x  -> eval env x
                     Nothing -> error $ "no value binded to " ++ n
eval _ t = t


betaReduction :: Env -> Expr -> Expr
betaReduction = undefined

alphaConvertion :: Env -> Expr -> Expr
alphaConvertion = undefined

etaReduction :: Env -> Expr -> Expr
etaReduction = undefined
