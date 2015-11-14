module HuttonsMonads where

import Prelude hiding (Maybe, Nothing, Just, (>>=), Monad)

--We can abstract both inc and square to map
inc = map (+1)
sqr = map (^2)

--Data type statements
data Expr = Val Int | Div Expr Expr
data Maybe a = Nothing | Just a

--Monad Maybe definition
instance Monad Maybe where
  return = Just

--Divide by zero helper
safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just (n `div` m)

{--
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                                Nothing -> Nothing
                                Just m -> safediv n m
--}

{--
seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing _ = Nothing
seqn _ Nothing = Nothing
seqn (Just x) (Just y) = Just (x, y)
--}

{--
eval (Val n) = Just n
eval (Div x y) = apply f (eval x `seqn` eval y)
                 where f (n, m) = safediv n m
--}

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

{--Abstract out the Maybe pattern, with >>= 'then' operator
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
            Nothing -> Nothing
            Just x -> f x
--}

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
(Just x) >>= f = f x

{--
eval (Val n) = Just n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))
--eval with sequencing operator
--could use do notation
--}

{--
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval x
                    safediv n m
--}
{--Ex.0: Show eval with >>= is equivalent to the original
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
(Just x) >>= f = f x

eval (Val n) = Just n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))

eval (Div x y) = (Nothing | eval x) (\n ->
                 (Nothing | eval y) (\m ->
                 safediv n m))
               = Just x (\n ->
                 Just y (\m ->
                 safediv n m))

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                   Nothing -> Nothing
                   Just n -> case eval y of
                               Nothing -> Nothing
                               Just m -> safediv n m
--}


{--Ex.1: Redefine seqn x y using do notation
seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing _ = Nothing
seqn _ Nothing = Nothing
seqn (Just x) (Just y) = Just (x, y)
--}
seqn x y = do x <- (Just x)
              y <- (Just y)
              Just (x, y)

{--
eval (Op x y z) = do n <- eval x
                     m <- eval y
                     o <- eval z
                     Op x y z
--}


--Declaring a monad typeclass
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad where
  -- return :: a -> Maybe a
  return x = Just x

  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x
