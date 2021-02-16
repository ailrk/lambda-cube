module Main where

{-@ Programming computable fucntions (PCF)
  A simple extension on typed lambda calculus.
@-}

main = undefined

type Name = String

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | Let Name Expr Expr
          | Lit Lit

data Lit = LInt Int
         | LBool Bool
