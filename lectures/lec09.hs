module Lec09 where

import Prelude hiding (Maybe, Nothing, Just)

--Defining custom types & typeclasses

------TYPES--------
--String is just a list of Char
type String = [Char]

--We can use types in functions
type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x, y) = (x - 1, y)

--We can use parameterised types
type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m, n) = m * n

copy :: a -> Pair a
copy x = (x, x)

--Type definitions can be nested but not recursive
type Trans = Pos -> Pos

--type Tree = (Int, [Tree]) NOT POSSIBLE!


------DATA TYPES---------

--data Bool = True | False

--We could read Bool as an abstract class that cannot
--be implemented on its own. True and False are both
--classes that extend Bool, but their type is Bool.

--True/False are called Constructors for Bool

--Type & Constructor names must begin with an uppercase char

--Data declarations are similar to context free grammars
--Declatations specify the values of a type
--Grammars specify the sentences of a language

--eg/True,False,Null:
data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

--eg/Shapes:
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--Circle and Rect can be viewed as functions that construct
--values of type shape

--We can use data types to make Nothing which is similar
--to an empty list:
data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)


-----RECURSIVE TYPES-----
data Nat = Zero | Succ Nat
--eg:
--Zero
--Succ Zero
--Succ (Succ Zero)
--etc...

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


-------TYPECLASSES-------
--Such as when building arithmetic expressions
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--Could define as a fold with Val replaced by id
--Add replaced by (+) and mul with (*)
--eval = fold id (+) (*)

data Tree = Leaf Int | Node Tree Int Tree

--Find if a given integer exists in a binary tree
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n
                      || occurs m l
                      || occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

--More efficient definition of occurs
occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n) = n == m
occurs' m (Node l n r) | n == m = True
                       | m > n = occurs m r
                       | m < n = occurs m l
