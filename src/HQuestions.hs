module HQuestions where

import Control.Arrow ((&&&))
import System.Random
import Data.List (nub, sortBy, group, sort)
import Data.Function (on)
import Control.Monad (replicateM)

h1 :: [a] -> a
h1 = last

h2 :: [a] -> a
h2 = last . init

h3 :: [a] -> Int -> a
h3 xs n = xs !! (n-1)

h4 :: [a] -> Int
h4 = foldr (const (+1)) 0

h5 :: [a] -> [a]
h5 = foldl (\acc x -> x : acc) []

h6 :: Eq a => [a] -> Bool
h6 xs = xs == h5 xs 

data NestedList a = Elem a | List [NestedList a]
h7 :: NestedList a -> [a]
h7 (Elem a) = [a]
h7 (List as) = concatMap h7 $ as

h8 :: Eq a => [a] -> [a]
h8 = fmap head . h9

h9 :: Eq a => [a] -> [[a]]
h9 = foldr f []
  where f x [] = [[x]]
        f x ((a:ac):acc) = if a == x then (x:a:ac):acc
                                     else [x]:(a:ac):acc

h10 :: Eq a => [a] -> [(Int, a)]
h10 = fmap (length &&& head) . h9

data SingleOrMultiple a = Single a | Multiple Int a deriving (Show)
h11 :: Eq a => [a] -> [SingleOrMultiple a]
h11 = fmap check . h9
  where check [x] = Single x
        check xs@(x:_)  = Multiple (length xs) x  

h12 :: [SingleOrMultiple a] -> [a]
h12 = concatMap change
  where change (Single x)     = [x]
        change (Multiple n x) = replicate n x

h13 :: Eq a => [a] -> [SingleOrMultiple a]
h13 = foldr change []
  where change x []                      = [Single x]
        change x acc@((Single y):ac)     = if x == y
          then (Multiple 2 x):ac
          else (Single x):acc
        change x acc@((Multiple n y):ac) = if x == y
          then (Multiple (n+1) y):ac
          else (Single x):acc

h14 :: [a] -> [a]
h14 = concatMap (replicate 2)

h15 :: [a] -> Int -> [a]
h15 xs n = xs >>= replicate n

h16 :: [a] -> Int -> [a]
-- h16 xs n = fmap snd . filter ((/=0) . flip mod n . fst) . zip [1..] $ xs
h16 xs n = [c | (i, c) <- zip [1..] xs, mod i n /= 0]

h17 :: [a] -> Int -> ([a], [a])
-- h17 = flip splitAt
h17 [] _ = ([], [])
h17 li@(x:xs) n | n <= 0    = ([], li)
                | otherwise = let (a, ac) = h17 xs (n-1)
                              in (x:a, ac)

h18 :: [a] -> Int -> Int -> [a]
h18 xs start stop = [xs !! (i-1) | i <- [start .. stop]]

h19 :: [a] -> Int -> [a]
h19 xs n = b ++ a
  where (a, b) = h17 xs (if n > 0 then n else length xs + n)

h20 :: Int -> [a] -> (a, [a])
h20 n xs = (last a, init a ++ b)
  where (a, b) = h17 xs (if n > 0 then n else length xs + n)

h21 :: a -> [a] -> Int -> [a]
h21 x xs n = r ++ (x:l)
  where (r, l) = h17 xs (if n > 0 then n-1 else length xs + n - 1)

h22 :: Int -> Int -> [Int]
h22 start stop = [start .. stop]

h23 :: [a] -> Int -> IO [a]
h23 xs n = do
  g <- getStdGen
  let index = take n $ randomRs (0, length xs - 1) g
  return [xs !! i | i <- index]

h24 :: Int -> Int -> IO [Int]
h24 num stop = do
  g <- getStdGen
  return . take num $ randomRs (1, stop) g

h25 :: Eq a => [a] -> IO [a]
-- h25 xs = do
--   g <- getStdGen
--   return . take (length xs) . nub $ [xs !! i | i <- randomRs (0, length xs - 1) g]
--too stupid
h25 [] = return []
h25 xs = do
  ind <- randomRIO (0, length xs-1)
  let (as, b:bs) = h17 xs ind
  rest <- h25 (as++bs)
  return (b : rest)

h26 :: Int -> [a] -> [[a]]
h26 0 _  = [[]]
h26 _ [] = [] -- very very important here
h26 n (x:xs) = ((x:) <$> h26 (n-1) xs) ++ (h26 n xs)

h27 :: [Int] -> [a] -> [[[a]]]
h27 [] _ = [[]]
h27 _ [] = []
h27 nl@(n:ns) xs =
  [ (li:gs) | (li, ri) <- change n xs, gs <- h27 ns ri ]
  where change :: Int -> [a] -> [([a], [a])]
        change 0 xs = [([], xs)]
        change _ [] = []
        change m (y:ys) = ((\(z, zs) -> (y:z, zs)) <$> change (m-1) ys) ++ 
                          ((\(z, zs) -> (z, y:zs)) <$> change m     ys)

h28 :: [[a]] -> [[a]]
h28 = sortBy (compare `on` length)

h28' :: [[a]] -> [[a]]
h28' xs = 
  let table = map (head &&& length) . group . sort . map length $ xs
      getFre x = case lookup (length x) table of
        Just fre -> fre
        Nothing  -> error "This should not happen" 
  in sortBy (compare `on` getFre) xs

h31 :: Integer -> Bool
h31 n = n `elem` (takeWhile (<=n) primes)
  where primes = 2 : 3 : 5 : (filter check [7,9..])
        check m = all ((/=0) . mod m) (takeWhile (<= (round . sqrt . fromIntegral $ m)) primes)

h32 :: Integer -> Integer -> Integer
h32 a b = if b == 0
  then abs a
  else h32 b (mod a b)

h33 :: Integer -> Integer -> Bool
h33 a b = (gcd a b) == 1

h34 :: Integer -> Int
h34 n = length . filter (h33 n) $ [1..n-1]

h35 :: Integer -> [Integer]
h35 = helper
  where primes = 2 : 3 : 5 : (filter check [7,9..])
        check m = all ((/=0) . mod m) (takeWhile (<= (round . sqrt . fromIntegral $ m)) primes)
        helper 1 = []
        helper m = 
          let n':_ = dropWhile ((/=0) . mod m) primes
          in n':(h35 (div m n'))

h36 :: Integer -> [(Integer, Int)]
h36 = map (head &&& length) . group . h35

h37 :: Integer -> Integer
h37 n = product [(p-1) * p^(m-1) | (p, m) <- (h36 n)] -- more effect

h39 :: Integer -> Integer -> [Integer]
h39 start stop = takeWhile (<= stop) (dropWhile (< start) primes)
  where primes = 2 : 3 : 5 : (filter check [7,9..])
        check m = all ((/=0) . mod m) (takeWhile (<= (round . sqrt . fromIntegral $ m)) primes)

h40 :: Integer -> (Integer, Integer)
h40 n = head [(m, k) | m <- thisprimes, let k = n - m, k `elem` thisprimes]
  where primes = 2 : 3 : 5 : (filter check [7,9..])
        check m = all ((/=0) . mod m) (takeWhile (<= (round . sqrt . fromIntegral $ m)) primes)
        thisprimes = takeWhile (<=n) primes 

h41 :: Integer -> Integer -> [(Integer, Integer)]
h41 start stop = [h40 k | k <- [start .. stop], even k]

h41' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
h41' start stop limit = filter ((> limit) . fst) . map h40 . filter even $ [start .. stop]

not' :: Bool -> Bool
not' True = False
not' _ = True
and', or', nand',nor',equ',xor',impl' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
or' False False = False
or' _ _ = True
nand' a b = not' (and' a b)
nor' a b = not' (or' a b)
equ' True True = True
equ' False False = True
equ' _ _ = False
xor' a b = not' (equ' a b)
impl' a b = or' b (not' a)
h47 :: (Bool -> Bool -> Bool) -> IO ()
h47 f = mapM_ putStrLn $ [show a ++ " " ++ show b ++ " " ++ show (f a b) | 
                          a <- [True, False],
                          b <- [True, False]]
infixl 4 `or'`
infixl 6 `and'`
infixl 3 `equ'`

h48 :: Int -> ([Bool] -> Bool) -> IO ()
h48 n f = mapM_ putStrLn [toStr args ++ " => " ++ show (f args)| args <- replicateM n [True, False]]
  where toStr = unwords . map space
        space True  = "True "
        space False = "False"

h49 :: Int -> [String]
h49 n = replicateM n "01" -- maybe wrong, if the order matters

h50 = undefined
