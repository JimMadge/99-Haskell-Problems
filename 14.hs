-- Duplicate the elements of a list.

-- Here is more than one solution
duplicate [] = []
-- duplicate (x:xs) = replicate 2 x ++ duplicate xs
-- duplicate xs = concatMap (replicate 2) xs
duplicate (x:xs) = [x, x] ++ duplicate xs

main = do
    print $ duplicate [1, 2, 3]
