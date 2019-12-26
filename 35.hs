-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.

-- isPrime from problem 31 solutions
isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

-- Find all UNIQUE prime factors, this is not the solution
-- primeFactors n = [x | x <- primes, mod n x == 0]
--     where primes = [x | x <- [1..(floor . sqrt $ fromIntegral n)], isPrime x]

-- Find the smallest prime factor of n by taking the first prime which divides
-- exactly into n
smallestPrimeFactor :: Int -> Int
smallestPrimeFactor n
    | isPrime n = n
    | otherwise = head [x | x <- primes, mod n x == 0]
        where primes = [x | x <- (2:3:[x + i | x <- [6,12..], i <- [-1,1]]), isPrime x]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = (smallest) : (primeFactors $ quot n smallest)
    where smallest = smallestPrimeFactor n

main = do
    print $ primeFactors 315
