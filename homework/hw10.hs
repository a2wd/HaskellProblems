--Extra homework problems for lecture 10
removeone x [] = []
removeone x (y:ys)
  | x == y = ys
  | otherwise = y : removeone x ys

isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)
