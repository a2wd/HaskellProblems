module HelloWorld where

--Learning about the IO monad; IO a
--type of actions that return a value of type a
--eg: IO Char returns a char
--IO ()[only side effects, void function, () is empty tuple]
--gethar :: IO Char
--putChar :: Char --> IO ()
a :: IO (Char, Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x, y)

--Derived primitives
getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

--Build up into interactive programs
strlen :: IO ()
strlen = do putStr "Enter a word: "
            xs <- getLine'
            putStr "The string is "
            putStr (show (length xs))
            putStrLn " characters long"
