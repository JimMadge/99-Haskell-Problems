-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.

-- Primefactors from problem 35 solutions
primeFactors 1 = []
primeFactors n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ primeFactors $ div n prime

collect [] = []
collect xs = collect' 1 xs

collect' n (x:xs)
    | xs == [] = [(x, n)]
    | x == head xs = collect' (n+1) xs
    | otherwise = (x, n): collect' 1 xs

primeFactorsMult = collect . primeFactors

main = do
    print $ primeFactorsMult 315
