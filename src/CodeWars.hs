module CodeWars where

-- permutations
selection :: [a] -> [(a, [a])]
selection [] = []
selection (x : xs) = (x, xs):[(y, x:ys) | (y, ys) <- selection xs]
permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x : ys' |
                  (x, ys) <- selection xs,
                  ys' <- permutation ys]
