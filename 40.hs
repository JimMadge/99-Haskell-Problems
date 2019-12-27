-- Goldbach's conjecture.

-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system). Write a predicate to
-- find the two prime numbers that sum up to a given even integer.
-- isPrime from problem 31 solutions

isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

goldbach n = goldbach' n primes
    where primes = [x | x <- [1..], isPrime x]

goldbach' n (x:xs)
    | isPrime (n-x) = (x,n-x)
    | otherwise = goldbach' n xs
