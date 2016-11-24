module CodeWars () where

import Data.Array
-- permutations
selection :: [a] -> [(a, [a])]
selection [] = []
selection (x : xs) = (x, xs):[(y, x:ys) | (y, ys) <- selection xs]
permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x : ys' |
                  (x, ys) <- selection xs,
                  ys' <- permutation ys]


-- reverse polish calculator
calc :: String -> Double
calc = head . foldl eval [0] . map par . words
  where eval exp (Number n) = n : exp
        eval (a:b:exp) (Op f) = f b a : exp

par :: String -> Expr Double
par "+" = Op (+)
par "-" = Op (-)
par "/" = Op (/)
par "*" = Op (*)
par x = Number (read x)

data Expr t = Number t | Op (t -> t -> t)

-- partition number theory
explosiveSum :: Int -> Integer
explosiveSum n | n < 0 = 0 
explosiveSum n = ar !! (fromIntegral n)
    where ar = 1:1:(map helper [2..])
          helper l =
            let ks  = takeWhile ((>=0) . fst) [(m, (ar !! m) * (-1)^(k-1))| k <- [1..], let m = l - (div (k*(3*k-1)) 2) ]
                ks' = takeWhile ((>=0) . fst) [(m, (ar !! m) * (-1)^(k+1))| k <- [1..], let m = l - (div (k*(3*k+1)) 2) ]
            in sum (map snd ks) + sum (map snd ks')


-- last digit of  a huge number
lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit as = read . (:[]) . last .show . foldr1 (^) $ as
