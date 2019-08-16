-- Split a list into two parts; the length of the first part is given.

split xs n = (take (n) xs, drop n xs)

main = do
    print $ split "abcdefghik" 3
