n = a `div` length xs
    where
          a = 10
          xs = [1,2,3,4,5]

qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
    where x = maximum xs
    	  smaller = [a | a <- xs, a < x]
          larger = [b | b <- xs, b >= x]

qsort2 [] = []
qsort2 (x:xs) = reverse (qsort2 smaller ++ [x] ++ qsort2 larger)
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x] 

qsort3 [] = []
qsort3 (x:xs) = qsort3 larger ++ qsort3 smaller ++ [x]
    where x = minimum xs
    	  smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x] 
