module HW09 where

import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero | Succ Nat deriving Show

--Ex0
-- answer a
natToIntegera Zero = 0
natToIntegera (Succ n) = natToIntegera n + 1
-- answer b
natToIntegerb (Succ n) = natToIntegerb n + 1
natToIntegerb Zero = 0
-- answer c
natToIntegerc n = natToIntegerc n
-- answer d
natToIntegerd (Succ n) = 1 + natToIntegerd n
natToIntegerd Zero = 0
-- answer e
natToIntegere Zero = 1
natToIntegere (Succ n) = (1 + natToIntegere n) - 1
-- answer f
natToIntegerf = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]
-- answer g
natToIntegerg :: Nat -> Integer
natToIntegerg = \n -> genericLength [c | c <- show n, c == 'S']
-- answer h
-- natToIntegerh :: Nat -> Integer
-- natToIntegerh = \n -> length [c | c <- show n, c == 'S']

--EX1
-- answer a
integerToNata 0 = Zero
integerToNata (n+1) = Succ (integerToNata n)
-- answer b
integerToNatb 0 = Succ Zero
integerToNatb n = (Succ (integerToNatb n))
-- answer c
integerToNatc n = product [(unsafeCoerce c) :: Integer | c <- show n]
-- answer d
integerToNatd n = integerToNatd n
-- answer e
integerToNate (n+1) = Succ (integerToNate n)
integerToNate 0 = Zero
-- answer f
integerToNatf (n+1) = let m = integerToNatf n in Succ m
integerToNatf 0 = Zero
-- answer g
integerToNatg = head . m
    where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
    }
-- answer h
-- integerToNath :: Integer -> Nat
-- integerToNath = \n -> genericLength [c | c <- show n, isDigit c]

--Ex2
-- answer a
adda Zero n = n
adda (Succ m) n = Succ (adda n m)
-- answer b
addb (Succ m) n = Succ (addb n m)
addb Zero n = n
-- answer c
addc Zero n = Zero
addc (Succ m) n = Succ (addc m n)
-- answer d
addd (Succ m) n = Succ (addd m n)
addd Zero n = Zero
-- answer e
adde n Zero = Zero
adde n (Succ m) = Succ (adde n m)
-- answer f
addf n (Succ m) = Succ (addf n m)
addf n Zero = Zero
-- answer g
addg n Zero = n
addg n (Succ m) = Succ (addg m n)
-- answer h
addh n (Succ m) = Succ (addh m n)
addh n Zero = n

--Ex3
-- answer a
multa Zero Zero = Zero
multa m (Succ n) = adda m (multa m n)
-- answer b
multb m Zero = Zero
multb m (Succ n) = adda m (multb m n)
-- answer c
multc m Zero = Zero
multc m (Succ n) = adda n (multc m n)
-- answer d
multd m Zero = Zero
multd m n = adda m (multd m (Succ n))

--Ex4
-- original statement
--data Ordering = LT | EQ | GT
--compare :: (Ord a) => a -> a -> Ordering
--data Tree = Leaf Integer | Node Tree Integer Tree
--occurs :: Integer -> Tree -> Bool

-- -- answer a
-- occursa m (Leaf n) = m == n
-- occursa m (Node l n r) = case compare m n of
--     LT -> occursa m l
--     EQ -> True
--     GT -> occursa m r
-- -- answer b
-- occursb m (Leaf n) = m == n
-- occursb m (Node l n r) = case compare m n of
--     LT -> occursb m r
--     EQ -> True
--     GT -> occursb m l
-- -- answer c
-- -- occursc m (Leaf n) = compare m n
-- -- occursc m (Node l n r) = case compare m n of
-- --     LT -> occursc m l
-- --     EQ -> True
-- --     GT -> occursc m r
-- -- answer d
-- occursd m (Leaf n) = m == n
-- occursd m (Node l n r) = case compare m n of
--     LT -> occursd m l
--     EQ -> False
--     GT -> occursd m r
-- -- answer e
-- occurse m (Leaf n) = m == n
-- occurse m (Node l n r)
--     | m == n = True
--     | m < n = occurse m l
--     | otherwise = occurse m r
-- -- answer f
-- occursf m (Leaf n) = m == n
-- occursf m (Node l n r)
--     | m == n = True
--     | m > n = occursf m l
--     | otherwise = occursf m r
-- -- answer g
-- -- occursg m n = m == n
-- -- occursg m (Node l n r)
-- --     | m == n = True
-- --     | m < n = occursg m l
-- --     | otherwise = occursg m r
-- -- answer h
-- occursh m (Leaf n) = m == n
-- occursh m (Node l n r)
--     | m == n = False
--     | m < n = occursh m r
--     | otherwise = occursh m l

--Ex5
data Tree = Leaf Integer | Node Tree Tree
-- answer a
leavesa (Leaf x) = x
leavesa (Node l r) = leavesa l + leavesa r
balanceda (Leaf _) = True
balanceda (Node l r) = abs (leavesa l - leavesa r) <= 1 || balanceda l || balanceda r
-- answer b
-- leavesb (Leaf _) = True
-- leavesb (Node l r) = leavesb l + leavesb r
-- balancedb (Leaf _) = True
-- balancedb (Node l r) = abs (leavesb l - leavesb r) <= 1
-- answer c
-- leavesc (Leaf _) = True
-- leavesc (Node l r) = leavesc l + leavesc r
-- balancedc (Leaf _) = True
-- balancedc (Node l r) = abs (leavesc l + leavesc r) <= 1
-- answer d
leavesd (Leaf _) = 1
leavesd (Node l r) = leavesd l + leavesd r
balancedd (Leaf _) = True
balancedd (Node l r) = abs (leavesd l - leavesd r) <= 1 && balancedd l && balancedd r

--Ex6
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

--Ex7
data Expr = Add Expr Expr | Val Int
prob7 = Add (Val 1) (Val 2)

--Ex9
-- instance Monad Maybe where
--   return x = Just x
--   Nothing >>= _ = Nothing
--   (Just x) >>= f = f x

--Ex10
-- instance Monad mwah where
--   return x = [x]
--   xs >>= f = concat (map f xs)

--Ex12
fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)
