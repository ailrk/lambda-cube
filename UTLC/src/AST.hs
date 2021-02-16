module AST where

import qualified Data.Vector as V

-- we use deburjin index to encode binder. so Env only needs to
-- be an index.
data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr

  | Lit Value

  | Let Expr Expr

  | Builtin Operator [Expr]

  | Bot
  deriving Show


data Value
  = VInt     Int
  | VBool    Bool
  | VClosure Env Expr
          -- ^ each closure has it's own environment
  deriving Show


data Operator
  = Add | Sub | Mult | Div
  | And | Or | Not
  deriving Show

type Env = V.Vector Value
