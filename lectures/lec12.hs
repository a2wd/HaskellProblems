module Lec12 where

--Reasoning about programs
--From Grant Hutton's Programming in Haskell, ch.12
data Expr = Val Int | Add Expr Expr
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s             = s
exec (PUSH n:c) s     = exec c (n:s)
exec (ADD:c) (m:n:s)  = exec c (n + m:s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]
