-- Euler project with haskell solutions
module Euler where

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

-- problem 18
inputp18 :: [[Integer]]
inputp18 =
  [ [75]
  , [95, 64]
  , [17, 47, 82]
  , [18, 35, 87, 10]
  , [20, 04, 82, 47, 65]
  , [19, 01, 23, 75, 03, 34]
  , [88, 02, 77, 73, 07, 63, 67]
  , [99, 65, 04, 28, 06, 16, 70, 92]
  , [41, 41, 26, 56, 83, 40, 80, 70, 33]
  , [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
  , [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
  , [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
  , [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
  , [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
  , [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]
p18 :: [[Integer]] -> Integer
p18 = maximum . foldl1' cojoin
  where
    cojoin acc new =
      let
        fstnum = zipWith (+) acc (init new)
        sndnum = zipWith (+) acc (tail new)
        part = (zipWith max (tail fstnum) (init sndnum))
      in head fstnum : part  ++ [last sndnum]

-- problem 67
p67 :: IO ()
p67 = do
  out <- readFile "src/assets/p67.txt"
  let out' = (map . map) read . map words . lines $ out
  print $ p18 out'


-- problem 19
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Show, Bounded, Ord, Enum)
type Day = Int
type Year = Int
type Date = (Year, Month, Day)
makeDate :: Year -> Month -> Day -> Maybe Date
makeDate year mon day =
  let days =
        if mon `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec]
        then 31
        else if mon `elem` [Apr, Jun, Sep, Nov]
        then 30
        else if (mod year 4 == 0 && mod year 100 /= 0) ||
                (mod year 400 == 0 && mod year 100 == 0)
        then 29
        else 28
  in if (0 < year) && (0 < day) && (day <= days)
     then Just (year, mon, day)
     else Nothing
p19 :: IO ()
p19 = do
  let join acc Nothing = acc
      join acc (Just x) = x:acc
      dates = foldl' join []
              (makeDate <$> [1900..2000] <*> [Jan .. Dec] <*> [1..31])
      week = cycle [1..7]
      output = filter (\((year, _, da), we) ->
                         year >= 1901 &&  da == 1 && we == 7)
               (zip (sort dates) week)
  print (length output)

-- p20
p20 :: Integer -> Integer
p20 upper =
  sum . map (\x -> read [x]) . show . product $ [1..upper]

-- p21
-- it's slow, maybe should remove lookup part
p21 :: Integer -> [Integer]
p21 n =
  let
    dvalue n' = sum . filter (\x -> mod n' x == 0) $ [1..n'-1]
    dvalues = map dvalue [1..n]
    pairAmi = filter (not . uncurry (==)) . zip [1..n] $ dvalues
    check (x, xpair) acc = case lookup xpair pairAmi of
      Nothing -> acc
      Just y -> if y == x then x : acc else acc
    numAmi = foldr check [] pairAmi
  in numAmi

-- problem 22
nameBase :: String -> Integer
nameBase = toInteger . sum . map ord'
  where ord' c = ord c - ord 'A' + 1
p22 :: IO ()
p22 = do
  text <- readFile "src/assets/p22.txt"
  let input = map nameBase . sort . read $ text
  print . sum $ (zipWith (*) input [1..])

-- problem 23
-- Too much time spent here
-- should avoid `elem` for large lists
-- try use Data.array (learn form google)
divisorsum :: Integer -> Integer
divisorsum num = if num == 1 then 1 else sum . init . divs $ num
  where divs n =
          let r = floor . sqrt . fromIntegral $ n
              (a, b) = unzip $ [(q, d) | q<-[1..r], let (d, r') = quotRem n q, r' == 0]
          in if r*r == n then a ++ (tail (reverse b))
                         else a ++ reverse b

p23' :: IO ()
p23'= do
  let upper = 28123
      aboundNumbers = filter (\x -> x < divisorsum x) [1..upper]
      -- aboundSums = makesum <$> aboundNumbers <*> aboundNumbers
      out = filter (\x -> any (`elem` aboundNumbers) (map (\y -> y-x) aboundNumbers)) [1..upper]
  print (sum [1..upper] - (sum out))

p23 :: IO ()
p23 = do
  let
    upper = 28123
    aboundNumbers = listArray (1, upper) (map (\x -> x < divisorsum x) [1..upper])
    abounds = filter (aboundNumbers !) [1..upper]
    remainders x = map (x-) $ takeWhile (\y -> y <= x `quot` 2) abounds
    aboundSum = filter (any (aboundNumbers !) . remainders) [1..upper]
    out = (sum [1..upper]) - (sum aboundSum)
  print out

-- problem 24
p24 :: String
p24 = last . take 1000000 . sort . permutations $ ['0'..'9']

-- problem 25
fibs :: [Integer]
fibs = 0:1:(zipWith (+) fibs (tail fibs))
p25 :: IO ()
p25 = do
  let digitstest = (==1000) . length . show
      (_, a:_) = break (digitstest . snd) (zip [0..] fibs)
  print a

-- problem 26
fractions :: Integer -> String
fractions n =
  let go (x:xs) acc = if x `elem` acc then acc else go xs (x:acc)
      go [] acc = acc
  in case fromRationalRepetend Nothing (1 % n) of
       Right (sci, _) -> formatScientific Fixed Nothing sci
       Left (sci, _) -> formatScientific Fixed Nothing sci

p26 :: IO ()
p26 =
  -- putStrLn . show . maximumBy (compare `on` (length . fractions)) $ [1..1000]
  -- putStrLn . show . maximum . zip (map (length . fractions) [1..1000]) $ [1..]
  print . maximumBy (compare `on` (length . fractions)) $ [1..1000]

-- copy from github, this works too
-- but i can't figure out why
cycleLength :: Integer -> Integer
cycleLength n | even n = 0
              | n `rem` 5 == 0 = 0
              | otherwise = head [p | p <- [1..], (10^p - 1) `rem` n == 0]
p26' :: IO ()
p26' = print $ maximumBy (compare `on` cycleLength) [1,3..1000]

-- problem 27
isPrimes :: Array Integer Bool
isPrimes =
  let ar =  listArray (0, 1000000) (False:False:True:(map helper [3..1000000]))
      helper n = all (\x -> mod n x /= 0) [p | p <- [2..(floor . sqrt . fromIntegral $ n)], ar ! p]
  in ar


quadraticsForm :: Integer -> Integer -> Integer
quadraticsForm a b =
  let f x = x^2 + a * x + b
  in toInteger . length $ takeWhile (isPrimes !) (map (abs . f) [0..])

p27 :: IO ()
p27 = do
  let limit = 1000
      cojoin x y = ((x, y), quadraticsForm x y)
      out = cojoin <$> [(-limit) .. limit] <*> [(-limit) .. limit]
  print $ maximumBy (compare `on` snd) out

-- problem 28

p28 :: Int -> Int
p28 n =
  let split' :: [Int] -> [Int] -> [[Int]]
      split' (a:as) bs =
        let (acc,left) = splitAt a bs
        in acc : split' as left
      split' _ _ = []

      cycleGet :: Int -> [Int] -> [Int]
      cycleGet n xs =
        let (a, left) = splitAt n xs
        in if n > length xs then []
                            else (last a) : cycleGet n left
      ind = [1..n]
      lens = map (8*) ind
      oneSize = map (2*) ind
  in (+1) . sum . concat . zipWith cycleGet oneSize . split' lens $ [2..(2*n+1)^2]

-- prblem 29
p29 :: [Integer] -> [Integer] -> Int
p29 xs ys = length . nub $ (^) <$> xs <*> ys
-- p29 [2..100] [2..100]

-- problem 30
-- the fifth power of 9 * 6 = 354294 < 10^6, so the maximum bound is 10^6
p30 :: [Integer]
p30 = filter go [2..10^6]
  where go n = (==n) . sum . map (\x -> (read [x])^5) . show $ n
-- sum p30

-- problem 31
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- should walk through all possiable combinations for n
p31 :: Int -> Maybe Int
p31 n =
  let coins = [1,2,5,10,20,50,100,200]
      maxbound = map (\x -> [0 .. (div n x)]) coins
      cojoin = map helper . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
        where helper xs = case (unzip xs) of
                (a:_, b) -> (a, sum b)
                _ -> error "conflicts with condition"
      f (co, li) acc = cojoin $ g <$> li <*> acc
        where g li' (acc', fre) = (co*li'+acc', fre)
  in lookup n (foldr f [(0,1)] (zip coins maxbound))

-- problem 32 TODO
-- inspired by source code for permutations
-- selections :: [a] -> [(a, [a])]
-- selections [] = []
-- selections (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- selections xs]
-- group3 :: [a] -> [([a],[a],[a])]
-- group3 [] = [([],[],[])]
-- group3 [x] = [([],[],[x])]
-- group3 [x, y] = [([],[],[x,y]), ([],[],[y,x]), ([],[x],[y]), ([],[y],[x])]
-- group3 xs = concat [[(a:a', b:b', c'), (a', b', a:b:c'), (a', b:b', a:c'), (a:a',b',b:c')] |
--              (a, as) <- selections xs,
--              (b, bs) <- selections as,
--              (a', b', c') <- group3 bs]

-- toInt xs = sum $ zipWith (*) xs [10^i | i<- [0..]]
-- p32 = filter test . group3 $ [1..9]
--   where
--     notnull = not . null
--     test (as, bs, cs) = (notnull as) && (notnull bs) && (notnull cs) &&
--       length as <= length cs && length bs <= length cs &&
--       (toInt as) * (toInt bs) == (toInt cs)
-- test xs = [(x, y, zs) |
--            (x, ys) <- selections xs,
--            (y, zs) <- selections ys]

-- problem 34
-- since 9! = 362880*6 < 10^6, so the upper bound must be 10^6
p34 :: [Integer]
p34 = filter checkp34 [10 .. (10^6)]
  where factorial :: Integer -> Integer
        factorial n = product [1..n]
        checkp34 :: Integer -> Bool
        checkp34 n =
          (==n) . sum . map (factorial . read . (:[])) . show $ n

-- problem 35
-- circular isPrimes, note the isPrimes has just bounds [0,1000000]
p35 :: [Integer]
p35 = filter checkCicular (filter (isPrimes !) [2..1000000])
  where checkCicular :: Integer -> Bool
        checkCicular n = all (isPrimes !) (rotations n)
        rotations n =
          let ns = map (read . (:[])) . show $ n
              len = length ns
              toInt xs = sum $ zipWith (*) [10^i | i<-[0..]] (reverse xs)
              new ind =
                let (a, b) = splitAt ind ns
                in toInt (b ++ a)
          in map new [1..len-1]

-- problem 36
p36 :: Integer
p36 = sum . filter palindNum $ [1..1000000]
  where palindromic xs = xs == (reverse xs)
        palindNum n = all palindromic [showIntAtBase 10 intToDigit n "",
                                       showIntAtBase 2 intToDigit n ""]

-- problem 37
p37 :: [Integer]
p37 = take 11 . filter truncatablePrime . filter (isPrimes !) $ [11..]
  where truncatablePrime = all (isPrimes !) . deletes
        deletes n =
          let ns = map (read . (:[])) . show $ n
              goleft [] = []
              goleft al@(_:xs) = al : goleft xs
              goright = map reverse . goleft . reverse
              toInt xs = sum $ zipWith (*) [10^i | i<-[0..]] (reverse xs)
          in map toInt (goleft (tail ns) ++ goright (init ns))

-- problem 38
-- the maximum bound is 987654321
p38 :: [(Bool, Maybe String)]
p38 = sortBy (compare `on` snd) . filter fst . map pandigital $ [1..9876]
  where
    pandigital :: Int -> (Bool, Maybe String)
    pandigital n =
      let ns = map (show . (n*)) [1..9]
          out = dropWhile ((/="123456789") . sort) . map (concat . flip take ns) $ [2..length ns]
      in if null out then (False, Nothing) else (True, Just (head out))

-- problem 39
-- m must be bigger than 3
p39 :: Int -> (Int, Int)
p39 m =
  let tris n = [(a,b,c) | c <- [(div n 3) .. (div n 2)]
                        , b <- [(div n 4)..c]
                        , let a = n-c-b
                        , a < b
                        , 0 < a
                        , a^2+b^2==c^2]
  in maximumBy (compare `on` snd) . map (\x -> (x, length . tris $ x)) $ [1..m]

-- problem 40
-- simple, just concate the number
p40 :: Int
p40 =
  let ns = concatMap show [1..]
      ind = [1, 10, 100, 1000, 10000, 100000, 1000000]
  in product (map (digitToInt . (ns !!) . (\x -> x-1)) ind)

-- problem 41
-- time consume too much, so i just chek from take 1 numbers, take 2 numbers, and then stop at 7
p41 :: [Integer]
p41 = filter (\n -> n `elem` (takeWhile (<=n) isPrimes')) .
      map toInt . permutations . reverse . take 7 $ [1..9]
      -- note that permutations itself generate number in ordering if input are ordering
  where toInt xs = sum $ zipWith (*) [10^i | i<-[0..]] (reverse xs)
        isPrimes' = 2:3:(filter check [5,7..])
          where check n = all (\x -> mod n x /= 0) (takeWhile (< cir) isPrimes')
                  where cir = floor . sqrt . fromIntegral $ n


-- problem 42
p42 :: IO ()
p42 = do
  out <- readFile "src/assets/p42.txt"
  let ns = [(n+1)*n `div` 2 | n <- [1..]]
      inns n = (==n) . head . dropWhile (<n) $ ns
      wordValue = sum . map (\c -> ord c - ord 'A' + 1)
      content = filter inns . map wordValue . read $ out
  print $ length content

-- preblem 43
p43 :: Int
p43 = sum . map toInt . filter subdiv . permutations $ [0..9]
  where toInt xs = sum $ zipWith (*) [10^i | i<-[0..]] (reverse xs)
        subdiv xs = go (tail xs) [2,3,5,7,11,13,17]
          where go _ [] = True
                go (x:y:z:zs) (a:as) = rem (toInt (x:y:z:[])) a == 0 &&
                                       go (y:z:zs) as
                go _ _ = False

-- problem 44 TODO
-- pentagon  = map (\n -> div (n*(3*n-1)) 2) [1..]
-- checkpentagon n = (n `elem`) . takeWhile (<=n) $ pentagon
-- -- test (i, j) = (checkpentagon (i+j) || checkpentagon (i-j))

-- popTwo :: [(Int, Int)]
-- popTwo = [(i, j) | i <- pentagon
--                  , j <- takeWhile (<= (limit i)) pentagon
--                  , i < j]
--          where limit n = div (n+1) 3 -1

-- | Use doctest to test results
-- | this is just for init
--
-- prop> \(Small n) -> fib n == fib (n+2) - fib (n+1)
fib :: Int -> Int
fib n =
  let fibs = 0:1:(zipWith (+) fibs (tail fibs))
  in fibs !! n
