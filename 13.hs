-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.

data Item a = Single a | Multiple Int a
    deriving Show

encodeItem 1 a = Single a
encodeItem n a = Multiple n a

encode a count [] = []
encode a count (x:xs)
    | a == x = encode a (count+1) xs
    | otherwise = encodeItem count a : encode x 1 xs

encodeDirect [] = []
encodeDirect (x:xs) = encode x 1 xs
