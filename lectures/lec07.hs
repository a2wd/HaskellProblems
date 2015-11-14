module Lecture where

import Data.Char
import Control.Monad

{-- Variables Defined in Class --}
newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f =  error "You must implement (>>=)"

instance MonadPlus Parser where
   mzero =  P (\inp -> [])
   p `mplus` q =  P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

item :: Parser Char
item = P (\inp -> case inp of
    []     -> []
    (x:xs) -> [(x,xs)])

failure :: Parser a
failure = mzero    
{-- End of class-defined variables --}

--A sequence of parsers can be combined using 'do'
p :: Parser (Char, Char)
p = do  x <- item
        item
        y <- item
        return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
               return x
           else
               failure
