module OtherStaff where

import Data.List
import Data.Char
import Data.Array
import Data.Scientific (fromRationalRepetend, formatScientific, FPFormat(..))
import Data.Ratio ((%))
import Data.Function (on)
import Numeric (showIntAtBase)

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 100) <$> arbitrary

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
                      in all (not . (==0) . (n `rem`)) lessThann
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
primes :: [Integer]
primes = 2:3:(filter isprime [5,7..])
  where isprime m =
          let limit = floor . sqrt . fromIntegral $ m
              lessThan = takeWhile (<= limit) primes
          in all ((/=0) . (m `rem`)) lessThan

consecutivePrime :: Integer -> Integer
consecutivePrime n =
  let xli = takeWhile (<=n) primes
      cumsum [] = []
      cumsum (x:xs) = x : (map (+x) (cumsum xs))
      step xs = 
      step xs = filter (<=n) (reverse . cumsum $ xs)
      go xs =
        let out = dropWhile (`notElem` xli) (step xs)
        in if null out then go (tail xs) else head out
  in go xli
