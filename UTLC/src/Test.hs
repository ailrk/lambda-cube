module Test where


import System.IO
import Data.Vector
import Eval
import AST
import Syntax

v = fromList [1, 2, 3]

e0 = (Builtin Add [Lit (VInt 1), Lit (VInt 3)])

e1 = App (Lam (Builtin Add [Lit (VInt 1), Lit (VInt 2)])) (Lit (VInt 3))
e2 = App (Lam (Var 0)) (Lit (VInt 4))

e3e = App (Lam (Var 1)) (Lit (VInt 4))

-- ((\x -> x) (\x -> x)) 0
e4 = App (App (Lam (Var 0)) (Lam (Var 0))) (Lit (VInt 0))

-- (let x = 1 in x + x)
e5 = Let (Lit (VInt 2)) (Builtin Add [Var 0, Var 0])

-- (\f -> \g -> \h -> h) 0 1 2
e6 n = App (App (App (Lam (Lam (Lam (Var n))))
                     (Lit (VInt 0)))
                (Lit (VInt 1)))
           (Lit (VInt 2))

-- (let f x = x + 1
--      y g x = g (x + 2)
--   in y f 2)

e7 = Let (Lam (Builtin Add [Var 0, Lit (VInt 1)]))
         (Let (Lam (Lam (App (Var 1) (Builtin Add [Var 0, Lit (VInt 2)]))))
              (App (App (Var 0) (Var 1)) (Lit (VInt 2))))

test1 :: IO ()
test1 = do
  code <- readFile "data/std.utlc"
  case parseUTLC code of
    Left  n -> print n
    Right e -> print $ runUTLC e

