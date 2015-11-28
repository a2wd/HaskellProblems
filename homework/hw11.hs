fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fibs' n = fibs !! n

largeFib = head (dropWhile (<= 1000) fibs) 
