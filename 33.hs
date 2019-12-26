-- Determine whether two positive integer numbers are coprime. Two numbers are
-- coprime if their greatest common divisor equals 1.

coprime a b = gcd a b == 1

main = do
    print $ coprime 35 64
