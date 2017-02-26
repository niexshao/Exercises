module OtherStaff where

import Data.List
import Data.Char
import Data.Array
import Data.Scientific (fromRationalRepetend, formatScientific, FPFormat(..))
import Data.Ratio
import Data.Function (on)
import Numeric (showIntAtBase)

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 100) <$> arbitrary

primes :: [Integer]
primes = 2:3:(filter isprime [5,7..])
  where isprime m =
          let limit = floor . sqrt . fromIntegral $ m
              lessThan = takeWhile (<= limit) primes
          in all ((/=0) . (m `rem`)) lessThan

fact :: Integer -> [Integer]
fact n = filter ((==0) . (n `rem`)) (takeWhile (<n) primes)
 
-- problem 45
p45 :: [Integer]
p45 =
  -- too slow
  -- let tri = [div (n*(n+1)) 2 | n <- [1..] ]
  --     pen = [div (n*(3*n-1)) 2 | n <- [1..]]
  --     hex = [n*(2*n-1) | n <- [1..]]
  --     check xs n = let (x:_) = dropWhile (< n) xs in x == n
  -- in [n | n <- [1..]
  --       , check tri n
  --       , check pen n
  --       , check hex n]
  [h*(2*h-1) | pvalue <- map (\x -> 3*x^2 - x) [1..]
             , let delta = floor . sqrt . fromIntegral $ (1+4*pvalue)
             , let h = quot (1+delta) 4
             , delta^2 == 1+4*pvalue
             , rem (-1+delta) 2 == 0
             , rem (1+delta) 4 == 0]

-- problem 46
p46 :: [Integer]
p46 = [n | n <- [3,5..]
         , let xs = takeWhile (<=n) primes
         , check n xs]
      where primes = 2:3:(filter isprime [5,7..])
              where isprime n =
                      let limitn = floor . sqrt . fromIntegral $ n
                          lessThann = takeWhile (<= limitn) primes
                      in all ((/=0) . (n `rem`)) lessThann
            test y x = let minus = y - x
                           delta = sqrt . fromIntegral . (`div` 2) $ minus
                       in (even minus) && ((floor delta)^2 == (div minus 2))
            check m ys = (last ys) /= m && not (any (test m) ys)

-- problem 47
-- consume too much time, but it works fine ORZ

p47 :: Int -> [(Int, Int)]
p47 n =
  head . dropWhile test . groupBy ((==) `on` snd) . zip [1..] . map countFactors $ [1..]
  where test al@((_, m):_) = (m /= n) || (length al /= n)
        primes :: [Integer]
        primes = 2:3:(filter isprime [5,7..])
          where isprime m =
                  let limit = floor . sqrt . fromIntegral $ m
                      lessThan = takeWhile (<= limit) primes
                  in all ((/=0) . (m `rem`)) lessThan
        countFactors m =
          length . filter (\x -> rem m x == 0) . takeWhile (<m) $ primes

-- problem 48
-- this is cheating ....
-- thank haskell's Integer type
p48 :: Integer
p48 = sum $ map (\n -> read . reverse . take 10 . reverse . show $ n^n) [1..1000]

-- problem 50 TODO

-- consecutivePrime :: Integer -> Integer
-- consecutivePrime n =
--  let xli = takeWhile (<=n) primes
--      cumsum [] = []
--      cumsum (x:xs) = x : (map (+x) (cumsum xs))
--      step xs = 
--      step xs = filter (<=n) (reverse . cumsum $ xs)
--      go xs =
--        let out = dropWhile (`notElem` xli) (step xs)
--        in if null out then go (tail xs) else head out
--  in go xli


-- problem 52
-- permuted multiples
p52 :: [Integer] -> [Integer]
p52 ns = map fst . filter hep . map (\m -> (m, (*m) <$> ns)) $ [1..]
  where hep (x, ys) = let x' = sort . show $ x
                          ys' = map (sort . show) ys
                      in all (==x') ys'

-- problem 53
p53 :: Int
p53 = length . filter (>1000000). concatMap combine $ [1..100]
  where fact :: Integer -> Integer
        fact n = product [1..n]
        combine :: Integer -> [Integer]
        combine n = map comb [0..n] where
          comb i = fact n `div` (fact i * fact (n-i))

data Card =
  Card Int | Jack | Queen | King | Ace deriving (Eq, Ord, Show)

-- problem 64
p64 :: Int -> Int
p64 num = 
  length . filter snd . map f $ [1..num]
  where 
    gen s1 =
      let generate now@(n, a, b) ns old = 
            let w = (n-a^2) `div` b
                k = div (a+floor (sqrt (fromIntegral n))) w
            in if now `elem` old
                 then (ns, old)
                 else generate (n, k*w-a, w) (k:ns) (now:old)
      in generate s1 [] []
    periodSquare n =
      let n' = floor . sqrt . fromIntegral $ n
          (left, _) = gen (n, n',1)
      in if n'^2 == n
           then (n', [])
           else (n', left)
    f n = let (a, b) = periodSquare n
          in (a, odd . length $ b)

-- problem 65
p65 :: Int -> Int
p65 n = sum . map (\n -> read [n]) . show . denominator . compose . take n $ es
  where 
    es = 2:1:2:(concat [[1,1,2*k] | k <- [2..]])
    compose :: [Ratio Integer] -> Ratio Integer -- this is necessary, because int won't be enough to store the data
    compose = foldr (\x acc -> 1 / (x + acc)) 0

-- problem 66
solve :: Integer -> (Integer, Integer)
solve d =
  -- given d, output (x, y)
  head [(x, y) | y <- [1..], 
                 let delta = (d*y^2 + 1),
                 let x = floor . sqrt . fromIntegral $ delta,
                 x^2 == delta]
{-p66 :: Int -> [(Int, Int, Int)]-}
{-p66 num = sort . map de $ ns -}
{-  where ns = [n | n <- [1..num], (floor . sqrt . fromIntegral $ n)^2 /= n]-}
{-        de d = let (x, y) = solve d in (x, y, d) -}
{-        take3 (a, b, c) = c-}

-- problem 69
p68 :: Integer
p68 = maximum . filter ((==16) . length . show) . fmap convert . filter check . fmap reform . permutations $ [1..10]
  where  reform xs = 
           let (a, b) = splitAt 5 xs
           in rearange $ zip3' a b (tail b ++ [head b]) -- trick here, clockwise and starting from minimum
         zip3' (x:xs) (y:ys) (z:zs) = [x,y,z] : zip3' xs ys zs
         zip3' _ _ _ = []
         rearange xs = 
           let small = minimum . fmap head $ xs
               (a, b) = span ((/= small) . head) xs
           in b ++ a
         check ys =
           let y : yy = fmap sum ys
           in all (== y) yy
         convert = read . concatMap show . concat 

-- problem 69
p69 n = 
  foldl' (\al@(_, acc) x -> if f x > acc then (x, f x) else al) (0, 0) [2..n]
  where
    f m = (fromIntegral m) / (fromIntegral (phi m))
    phi m = length . filter (\q -> all ((/=0) . rem q) (fact m)) $ [1..m-1] 

-- | Just fun