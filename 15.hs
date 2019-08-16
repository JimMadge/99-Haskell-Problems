-- Replicate the elements of a list a given number of times.

replicate' [] _ = []
-- replicate' (x:xs) n = replicate n x ++ replicate' xs n
replicate' xs n = concatMap (replicate n) xs

main = do
    print $ replicate' "abc" 3
