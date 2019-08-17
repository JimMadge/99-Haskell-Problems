-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).

-- Only for positive shifts
-- rotate [] _ = []
-- rotate xs n
--     | n < 0 = error "negative n"
--     | n > length xs = error "n greater than length of list"
--     | n == 0 = xs
--     | n == length xs = xs
--     | otherwise = last ++ first
--         where (first, last) = splitAt n xs

-- Works for positive and negative, but a bit messy
-- rotate [] _ = []
-- rotate xs n
--     | abs n > length xs = error "n greater than length of list"
--     | n == 0 = xs
--     | abs n == length xs = xs
--     | n > 0 = last ++ first
--     | n < 0 = last' ++ first'
--         where (first, last) = splitAt n xs
--               (first', last') = splitAt (length xs + n) xs

-- Recursive solution inspired by the wiki solution
-- This is better because it allows to the number of rotations to be greater
-- than the length of the list.
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
    | n >= 0 = rotate (xs ++ [x]) (n-1)
    | n < 0 = rotate xs (length xs + n)

main = do
    print $ rotate ['a','b','c','d','e','f','g','h'] 3
    print $ rotate ['a','b','c','d','e','f','g','h'] (-2)
