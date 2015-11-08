--Church Numerals

type Church a = (a -> a) -> a -> a

zero = \s z -> z
one = \s z -> s z
two = \s z -> s (s z)
{-- We can simplify the above as follows:
	= \s z -> (s . s) z
	= \s -> s . s
--}

c2i x = x (+1) 0

c2i two = (\s z -> s (s z)) (+1) 0
		= (+1) ((+1) 0)
		= (+1) 1 = 2

mul x y = \s z -> x (y s) z
{-- We can simplify as follows:
        = \s z -> (x . y) s z
        = x . y
--}