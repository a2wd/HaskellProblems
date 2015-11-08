-- Ex 0
-- Choose the equivalent of [f x | x <- xs, p x]
-- Apply map on a filtered list

mapfilter f p xs = map f (filter p xs)
mapfiltercomp f p xs = [f x | x <- xs, p x]

-- Ex 1
-- Choose all options that implement all :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)
--all2 p xs = map p (and xs)
all3 p = and . map p
all4 p = not . any (not . p)
--all5 p = map p . and
all6 p xs = foldl (&&) True (map p xs)
all7 p xs = foldr (&&) False (map p xs)
all8 p = foldr (&&) True . map p

-- Ex 2
-- Choose all options that implement any :: (a -> Bool) -> [a] -> Bool
--any1 p = map p . or
any2 p = or . map p
any4 p = not . null . dropWhile (not . p)
any5 p = null . filter p
any6 p xs = not (all (\x -> not (p x)) xs)
any7 p xs = foldr (\x acc -> (p x) || acc) False xs
any8 p xs = foldr (||) True (map p xs)

-- Ex 5
-- Choose the option that implements map
map1 f = foldr (\x xs -> xs ++ [f x]) []
map2 f = foldr (\x xs -> f x ++ xs) []
map3 f = foldl (\xs x -> f x : xs) []
map4 f = foldl (\xs x -> xs ++ [f x]) []
map5 f = foldr (\x xs -> f x : xs) []

-- Ex 7
-- Choose the solution that implements dec2int
dec2int = foldr (\x y -> 10 * x + y) 0
dec3int = foldl (\x y -> x + 10 * y) 0
dec4int = foldl (\x y -> 10 * x + y) 0
dec5int = foldr (\x y -> x + 10 * y) 0

-- Ex 8
-- Find the error in sumsqreven, built with compose
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- Ex 9
-- Find the correct definition of curry
curry1 f = \ (x, y) -> f x y
curry2 f = \ x y -> f (x, y)

-- Ex 11, 12 & 13
-- Define chop8, map & iterate with unfold
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

chop8 = unfold null (take 8) (drop 8)
unfoldmap f = unfold null (f . head) tail
unfolditerate f = unfold (const False) id f


-- Ex 14
-- Identities of composed functions
f = (+5)
g = (+7)
h = (+10)
lhs = f . (g . h)
rhs = (f . g) . h

-- Ex 24
-- Choose the overloaded function
 overloadedf x = x > 3










