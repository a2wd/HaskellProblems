-- Caesar.hs
-- Functional programming in Haskell reading
-- A module to encode and break Caesar ciphers

module Caesar where

import Data.Char

--Helper functions to encode to and from int/char
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr(ord 'a' + i)

--Second order helper function to shift single characters
shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
					| otherwise = c

--Encoding function to implement string rotation 
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--To crack the Caesar cipher, letter frequency can be used
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

--Find the frequency of a letter in the string
--Find the percentage of one int in another
percent :: Integer -> Integer -> Float
percent n m = (fromInteger n / fromInteger m) * 100

--Sum of lowercase characters in a string
lowers :: String -> Int
lowers xs = length [x | x <- xs, x<='z', x>='a']

--Sum of occurrences of c in string xs
count :: Char -> String -> Int
count c xs = length [x | x <- xs, c==x]

--Find the frequencies of each alphabet-character in a string
freq :: String -> [Float]
freq xs = [percent (toInteger(count x xs)) (toInteger n) | x <- ['a'..'z']]
					where n = lowers xs

--Crack the cipher by comparing letter frequencies with expected
--frequencies: the chi-square statistic
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e|(o,e)<-zip os es]

--To automate the chi-square calcs, rotate the lists
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

--Helper function for crack
--Positions returns a list of positions of a character in a string
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..length(xs)-1], x == x']

--Crack by finding the lowest chi-square as the rotation factor
crack :: String -> String
crack xs = encode (-factor) xs
	where
		factor = head(positions (minimum chitab) chitab)
		chitab = [chisqr(rotate n table') table | n <- [0..25]]
		table' = freq xs
