------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (r :> _) = r

children :: Rose a -> [Rose a]
children (_ :> rs) = rs

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

--Ex0
--Given: tree = 'x' :> map (flip (:>) []) ['a'..'x']
--What is: length $ children tree

--Ex1
--Given: tree = 'x' :> map (\c -> c :> []) ['a'..'A']
--What is: length (children tree)

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (_ :> []) = 1
size (_ :> rs) = 1 + (sum $ map size rs)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> rs) = sum $ map leaves rs

--Ex3
--Given: tree = 1 :> map (\c -> c :> []) [1..5]
--WHat is: size tree

--Ex4
--Given: tree = 1 :> map (\c -> c :> []) [1..5]
--What is: size . head . children $ tree

--Ex5
--Given: tree = 1 :> map (\c -> c :> []) [1..5]
--What is: leaves tree

--Ex6
--Given: tree = 1 :> map (\c -> c :> []) [1..5]
--What is: product (map leaves (children tree))

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (r :> []) = (f r) :> []
  fmap f (r :> rs) = (f r) :> map (fmap f) rs

--Ex8
--Given: tree = 1 :> map (\c -> c :> []) [1..5]
--What is: size (fmap leaves (fmap (:> []) tree))

--Ex9
--Given: f r = fmap head $ fmap (\x -> [x]) r
--What is a valid type signature for f?

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum n) (Sum m) = Sum (n + m)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product n) (Product m) = Product (n * m)

unSum :: Sum a -> a
unSum (Sum n) = n
unProduct :: Product a -> a
unProduct (Product n) = n

--Ex11
--Evaluate: unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))

--Ex12
--What is the type of: Sum 3 `mappend` Sum 4

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap f xs = fold $ fmap f xs
 
instance Foldable [] where
  fold = foldr (mappend) mempty 

instance Foldable Rose where
  fold (r :> []) = r `mappend` mempty
  fold (r :> rs) = r `mappend` fold (map fold rs)

--Ex14
--Given: tree = 1 :> [2 :> [], 3 :> [4 :> []]] 
--and: tree' = fmap Product tree
--what is the result of: unProduct $ fold tree'
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

--Ex16
--Given: tree = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
--What is the result of: unSum $ foldMap Sum tree

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum xs = unSum $ foldMap Sum xs
fproduct xs = unProduct $ foldMap Product xs

--Ex19
--What is the result of: fsum xs

--Ex20
--What is the result of: fproduct xs

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

