-- Higher order functions

--Map function as a list-comprehension and a recursive function
mapL f xs = [f x | x <- xs]

mapR _ [] = []
mapR f (x:xs) = f x : mapR f xs

--Same for filter function
filterL p xs = [x | x <- xs, p x]

filterR _ [] = []
filterR p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

--Using foldr to define other functions
reverse' = foldr' (\x xs -> xs ++ [x]) []

-- We could rewrite the concatenation operator with foldr:
-- (++ ys) = foldr (:) ys

--Composition
--Composing functions is another important fundamental concept
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

--Library functions with foldr
allL :: (a -> Bool) -> [a] -> Bool
allL p xs = and [p x | x <- xs]

