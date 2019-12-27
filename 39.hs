-- A list of prime numbers.

-- Given a range of integers by its lower and upper limit, construct a list of
-- all prime numbers in that range.

-- isPrime from problem 31 solutions
isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

primesR low high = [x | x <- [low..high], isPrime x]

main = do
    print $ primesR 10 20
