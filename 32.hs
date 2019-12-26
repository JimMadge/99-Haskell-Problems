-- Determine the greatest common divisor of two positive integer numbers

-- Naive implementation
-- gcd' a b
--     | a == b = a
--     | otherwise = gcd' (m-n) n
--     where
--         m = max a b
--         n = min a b

gcd' a b
    | b == 0 = a
    | otherwise = gcd' a (mod a b)

main = do
    print $ gcd' 36 63
