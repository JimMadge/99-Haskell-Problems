-- Determine whether a given integer number is prime.

isPrime :: Int -> Bool
isPrime n
    | n == 1 = False
    | n == 2 = True
    | n == 3 = True
    | mod n 2 == 0 = False
    | mod n 3 == 0 = False
    | otherwise = isPrime' n (ceiling $ sqrt $ fromIntegral n)

-- Naive implementation
-- isPrime' :: Int -> Int -> Bool
-- isPrime' n i
--     | i == 1 = True
--     | mod n i == 0 = False
--     | otherwise = isPrime' n (i-1)

-- Only check division by odd numbers
-- isPrime' :: Int -> Int -> Bool
-- isPrime' n l = not $ any (==0) $ map (mod n) $ takeWhile (<=l) [3,5..]

-- Only check division by numbers in the series 6k+/-1
isPrime' :: Int -> Int -> Bool
isPrime' n l = not $ any (==0) $ map (mod n) $ takeWhile (<=l) [x+y | x <- [6,12..], y <- [-1,1]]

main = do
    print $ isPrime 7
