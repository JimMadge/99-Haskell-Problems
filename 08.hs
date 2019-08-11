-- Eliminate consecutive duplicates of list elements.

-- Remove all leading elements equal to a from a list
removeAllLeading a [] = []
removeAllLeading a (x:xs)
    | a == x = removeAllLeading a xs
    | otherwise = x:(removeAllLeading x xs)

compress [] = []
compress (x:xs) = x:(removeAllLeading x xs)

-- A more consise solution from the wiki.
-- Here, the @ symbols lets us name the remainer of the list 'ys' with head 'y'
-- compress (x:ys@(y:_))
    -- | x == y    = compress ys
    -- | otherwise = x : compress ys
-- compress ys = ys

main = do
    print $ compress "aaaabccaadeeee"
