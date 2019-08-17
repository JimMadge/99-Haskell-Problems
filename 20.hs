-- Remove the K'th element from a list.

removeAt n xs =
    let (a, b) = splitAt n xs in
    (last a, init a ++ b)

main = do
    print $ removeAt 2 "abcd"
