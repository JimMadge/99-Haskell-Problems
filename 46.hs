-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.

-- A logical expression in two variables can then be written as in the
-- following example: and(or(A,B),nand(A,B)).

-- Now, write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.

and' True True = True
and' _ _ = False

or' True _ = True
or' _ True = True
or' False False = False

-- nand' = not . and'
nand' a b = not $ and' a b

-- nor' = not . or'
nor' a b = not $ or' a b

xor' True True = False
xor' a b = or' a b

impl' a b = or' (not a) b

equ' a b = not $ xor' a b

table f = putStrLn $ concatMap (++ "\n") [show a ++ "\t" ++ show b ++ "\t" ++ show (f a b) | a <- [True, False], b <- [True, False]]

main = do
    table (\a b -> (and' a (or' a b)))
