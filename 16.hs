-- Drop every N'th element from a list.

dropEvery [] _ = []
dropEvery _ 1 = []
dropEvery xs n = before ++ dropEvery after n
    where
        before = take (n-1) xs
        after = drop n xs

main = do
    print $ dropEvery "abcdefghik" 3
