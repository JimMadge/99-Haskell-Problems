-- Generate a random permutation of the elements of a list.
import System.Random
import Data.List

rndPermu [] _ = []
rndPermu xs gen = selected : rndPermu (delete selected xs) newgen
    where
        selected = xs !! index
        (index, newgen) = randomR (0, (length xs)-1) gen :: (Int, StdGen)

main = do
    gen <- getStdGen
    print $ rndPermu "abcdef" gen
