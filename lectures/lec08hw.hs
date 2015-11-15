-- module Hw07 where

--Homework Definitions
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

--Ex2
putStr1 [] = putChar '\n'
putStr1 xs = putStr' xs >> putStr1 ""

putStr2 [] = putChar '\n'
putStr2 xs = putStr' xs >> putChar '\n'

putStr3 [] = putChar '\n'
putStr3 xs = putStr' xs >>= \x -> putChar '\n'

--putStr4 [] = putChar '\n'
--putStr4 xs = putStr' xs >> \x -> putChar '\n'

putStr5 [] = putChar '\n'
putStr5 xs =  putStr' xs >> putStr' "\n"

putStr6 [] = putChar '\n'
putStr6 xs = putStr' xs >> putStr6 "\n"

putStr7 [] = putChar '\n'
putStr7 xs = putStr7 xs >> putStr' "\n"

--putStr8 [] = putChar "\n"
--putStr8 xs = putStr' xs >> putChar '\n'


--Ex3
getLine' = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

--Ex4
interact' f = do input <- getLine
                 putStrLn (f input)

--Ex5
-- answer a
-- sequence_'a [] = return []
-- sequence_'a (m : ms) = m >> \_ -> sequence_'a ms
-- answer b
sequence_'b [] = return ()
sequence_'b (m : ms) = (foldl (>>) m ms) >> return ()
-- answer c
sequence_'c ms = foldl (>>) (return ()) ms
-- answer d
sequence_'d [] = return ()
sequence_'d (m : ms) = m >> sequence_'d ms
-- answer e
sequence_'e [] = return ()
sequence_'e (m : ms) = m >>= \_ -> sequence_'e ms
-- -- answer f
-- sequence_'f ms = foldr (>>=) (return ()) ms
-- answer g
sequence_'g ms = foldr (>>) (return ()) ms
-- answer h
sequence_'h ms = foldr (>>) (return []) ms

--Ex6
a = putChar 'a'
b = putChar 'b'
eg = sequence1 [a,b]

sequence1 [] = return []
sequence1 (m:ms) = m >>= \a ->
                      do as <- sequence1 ms
                         return (a:as)

-- sequence2 ms = foldr func (return ()) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc = do x <- m
--                     xs <- acc
--                     return (x:xs)

-- sequence3 ms = foldr func (return []) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc = m : acc

-- sequence4 [] = return []
-- sequence4 (m:ms) = return (a:as)
--   where
--     a <- m
--     as <- sequence4 ms

sequence5 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x:xs)

-- sequence6 [] = return []
-- sequence6 (m:ms) = m >> \a -> do as <- sequence6 ms
--                                  return (a:as)

-- sequence7 [] = return []
-- sequence7 (m:ms) = m >>= \a ->
--   as <- sequence7 ms
--   return (a:as)

sequence8 [] = return []
sequence8 (m:ms) = do a <- m
                      as <- sequence8 ms
                      return (a:as)

--Ex7
mapM1 f as = sequence (map f as)

mapM2 f [] = return []
mapM2 f (a:as) = f a >>= \b -> mapM2 f as >>= \bs -> return (b:bs)

mapM3 f as = sequence_ (map f as)

-- mapM4 f [] = return []
-- mapM4 f (a:as) = f a >> \b -> mapM4 f as >> \bs -> return (b:bs)

-- mapM5 f [] = return []
-- mapM5 f (a:as) = do f a -> b
--                     mapM5 f as -> bs
--                     return (b:bs)

mapM6 f [] = return []
mapM6 f (a:as) = do b <- f a
                    bs <- mapM6 f as
                    return (b:bs)

mapM7 f [] = return []
mapM7 f (a:as) = f a >>= \b -> do bs <- mapM7 f as
                                  return (b:bs)

-- mapM8 f [] = return []
-- mapM8 f (a:as) = f a >>= \b -> do bs <- mapM8 f as
--                                   return (bs ++ [b])

--Ex8
-- answer a
filterM'a _ [] = return []
filterM'a p (x : xs) = do flag <- p x
                          ys <- filterM'a p xs
                          return (x : ys)
-- answer b
filterM'b _ [] = return []
filterM'b p (x : xs) = do flag <- p x
                          ys <- filterM'b p xs
                          if flag then return (x : ys) else return ys
-- answer c
filterM'c _ [] = return []
filterM'c p (x : xs) = do ys <- filterM'c p xs
                          if p x then return (x : ys) else return ys
-- answer d
filterM'd _ [] = return []
filterM'd p (x : xs) = do flag <- p x
                          ys <- filterM'd p xs
                          if flag then return ys else return (x : ys)

--Ex9
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (b : bs) = f a b >>= \a' -> foldLeftM f a' bs

--Ex10
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f b [] = b
foldRight f b (a : as) = f a $ foldRight f b as

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (a : as) = (foldRightM f b as) >>= \b' -> f a b'

--Ex11
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m
  = do x <- m
       return (f x)

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = m >>= \a -> return (f a)
