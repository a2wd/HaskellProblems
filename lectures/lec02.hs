import Prelude hiding ((&&))


double x = x * 2
palindrom xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)
add = \x -> (\y -> x + y)

funct x xs = take (x + 1) xs ++ drop x xs

e13a x y = x / y

e13b x y = x + y * y
