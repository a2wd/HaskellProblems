module Lec10 where

--The countdown problem
--Given five numbers and the operators (+, -, /, *)
--Find a random number

--Evaluating expressions:
data Op = Add | Sub | Mul | Div

--Symbolic application of operators
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--Check an operation is valid
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

--A data type for expressions
data Expr = Val Int | App Op Expr Expr

--How to evaluate an expressions
--Return a singleton list for success or an empty list on failure
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

--Choices -> All possible ways to choose 0 or more values from a list
choices :: [a] -> [[a]]
choices = error "Undefined"
--Values - return a list of all the values in an expressions
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

--Decide if an expression is a solution for a given list of source numbers
--and a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

--Split returns all possible ways of splitting a list into two non-empty parts
split :: [a] -> [([a], [a])]
split = error "Undefined"

--Return a list of all possible expressions
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                       l <- exprs ls,
                       r <- exprs rs,
                       e <- combine l r]

--Combine two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

--Return a list of all possible solutions
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                       e <- exprs ns',
                       eval e == [n]]

--This currently searches 33 million possiblities of which only around 5mill
--are valid

--We can improve this by checking for impossible expressions
type Result = (Expr, Int)

results :: [Int] -> [Result]
-- results ns = [(e, n) | e <- exprs ns,
--                        n <- eval e]

results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                          lx <- results ls,
                          ry <- results rs,
                          res <- combine' lx ry]

--where
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) |
                                            o <- [Add, Sub, Mul, Div],
                                            valid o x y]

--New solution function
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                      (e, m) <- results ns,
                      m == n]

--Can we do better? Yes, exploit that x * y == y * x and x * 1 = x
--Extend valid:
valid' Add x y = x <= y
valid' Mul x y = x <= y && x /= 1 && y /= 1
