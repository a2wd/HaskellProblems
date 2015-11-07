import Data.Char

sum100 = sum [x ^ 2 | x <- [1..100]]
sum1002 = foldl (+) (0) [x ^ 2 | x <- [1..100]]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

divides :: Int -> Int -> Bool
divides whole divisor = whole `mod` divisor == 0

divisors x = [d | d <- [1..x], x `divides` d]

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]


--Caesar Cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

let2intUpper :: Char -> Int
let2intUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
	| isLower c = int2let ((let2int c + n) `mod` 26)
	| isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
	| otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

xs = 1 : [x + 1 | x <- xs]

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]