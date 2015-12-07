module Hw13 where

--Definitions of fold
foldla1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

foldla2 f a bs = foldr (\a b -> f b a) a bs

foldla3 f = flip $ foldr (\a b g -> b (f g a)) id

--foldl4 = foldr . flip
