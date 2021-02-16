{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
module Eval where

-- untyped lambda calculus with let extension
-- special fuction print and read are added as io primitive.
-- a file based module system with name space is added.

import           AST
import qualified Data.Vector            as V
import           Syntax

import           Control.Monad.Except
import           Control.Monad.Identity
import           Data.Either
import           Data.Functor
import           Data.Maybe

data UTLCErr = UTLCErr String deriving Show
type UTLC m = (MonadError UTLCErr m)

runUTLC :: Expr -> Either UTLCErr Value
runUTLC = runIdentity . runExceptT . (eval newEnv)

eval :: UTLC m => Env -> Expr -> m Value
eval env = \case
  Var n         -> do
    case (env V.!? n) of
      Nothing -> throwError (UTLCErr ("var error, index: " ++ show n))
      Just v  -> return v

  Lam e         -> return $ VClosure env e

  App f e       -> do
    clos <- eval env f
    case clos of
      VClosure env' f' -> do
        e' <- eval env e
        eval (V.cons e' env') f'
      _ -> throwError (UTLCErr "application error")

  Let e1 e2     -> do
    value <- eval env e1
    eval (V.cons value env) e2

  Lit n         -> return n

  Builtin op xs -> traverse (eval env) xs >>= evalBuiltin op

  Bot           -> throwError (UTLCErr "bottom")


evalBuiltin :: UTLC m => Operator -> [Value] -> m Value
-- arithm
evalBuiltin Add  [VInt x, VInt y]   = return $ VInt (x + y)
evalBuiltin Sub  [VInt x, VInt y]   = return $ VInt (x - y)
evalBuiltin Mult [VInt x, VInt y]   = return $ VInt (x * y)
evalBuiltin Div  [VInt x, VInt y]   = return $ VInt (x `div` y)
-- logic
evalBuiltin And  [VBool x, VBool y] = return $ VBool (x && y)
evalBuiltin Or   [VBool x, VBool y] = return $ VBool (x || y)
evalBuiltin Not  [VBool x]          = return $ VBool (not x)
evalBuiltin op   xs                  =
  throwError (UTLCErr $ "Wrong builtin op: " ++ show op ++ " " ++ show xs)

newEnv :: Env
newEnv = V.empty
