-- Create a list containing all integers within a given range.

-- Using built in syntax
-- Only works if b > a
-- range a b = [a..b]

-- Using built in syntax, but works for a > b
-- range :: Integral a => a -> a -> [a]
-- range a b
--     | b > a = [a..b]
--     | otherwise = reverse [b..a]


-- With recursion
range :: Integral a => a -> a -> [a]
range a b
    | b > a = range' a b 1
    | otherwise = range' a b (-1)

range' start end direction
    | start == end = [end]
    | otherwise = start : range' (start + direction) end direction

main = do
    print $ range 4 9
