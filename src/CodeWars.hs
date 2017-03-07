{-# LANGUAGE FlexibleContexts #-}
module CodeWars () where

import Data.Array
import Data.List (nub, sort, (\\), transpose, groupBy, sortBy)
import qualified Data.Array.IArray as Arr
import Data.Array.IArray (IArray)
import Text.Parsec
import Data.Char (toUpper, isAlpha, isDigit)
import Data.Function (on)
import Data.Monoid (First(..))
import Data.Foldable (Foldable(..))
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

-- | The most imperative functional
-- check Imperative.hs
dividable n m = mod m n == 0
primes :: [Integer]
primes = 2:3:filter isPrime [5,7..]
  where isPrime n = 
          let li = takeWhile (<= (round . sqrt . fromIntegral $ n)) primes
          in length (filter (flip dividable n) li) == 0

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = map (\n -> (n, sum . filter (dividable n) $ xs)) (factors' xs)

divRepeat n m = last . takeWhile (\(_, rem) -> rem == 0) $ iterate (\(a, b) -> quotRem a n) (m, 0)
factors [] _ _ out = out
factors li n (m:remain) out =
  let new = filter (/=1) . map (fst . (divRepeat n)) $ li
  in if any (dividable n) li 
       then factors new m remain (n:out)
       else factors li m remain out
factors' li = reverse $ factors (filter (>1) . nub . map abs $ li) 2 (tail primes) []

data BVal = C Char | V Int
data Direction = ToR | ToL | ToU | ToD

-- befunge :: IArray (Int, Int) Char -> (Int, Int) -> [BVal] -> Direction -> [Char] -> [Char]
-- befunge stringmap (col, row) stack dir output = 
--   let now = stringmap Arr.! (col, row)
--       defaultPos = case Direction of
--         ToR -> (col+1, row)
--         ToU -> (col, row-1)
--         ToD -> (col, row+1)
--         ToL -> (col-1, row)
--       readop '+' = (+)
--       readop '-' = (-)
--       readop '*' = (*)
--       readop '\' = \a b -> if a == 0 then 0 else b / a
--       readop '`' = \a b -> if b > a then 1 else 0
--       par str | str `elem` "0123456789" =
--         befunge stringmap defaultPos (V (read str) : stack) dir output
--       par str | str `elem` "+-*/%`" =
--         let (V a) : (V b) : rem = stack
--         in befunge stringmap defaultPos (V (readop str a b) : rem) dir output
--       par str | str `elem` "><^v" =
--         case str of
--           '>' -> befunge stringmap (col+1, row) stack ToR output
--           '<' -> befunge stringmap (col-1, row) stack ToL output
--           '^' -> befunge stringmap (col, row-1) stack ToU output
--           'v' -> befunge stringmap (col, row+1) stack ToD output
--       par str | str `elem` "_|" =
--         let (a : rem) = stack
--         in case str of
--              '_' -> if a == 0 
--                       then befunge stringmap (col+1, row) rem ToR output
--                       else befunge stringmap (col-1, row) rem ToL output
--              '|' -> if a == 0
--                       then befunge stringmap (col, row+1) rem ToD output
--                       else befunge stringmap (col, row-1) rem ToU output
--       par '!' =
--         let (a:rem) = stock
--         in if a == 0
--       par '?' =
--       par ':'
--       par '"'
--       par '\'
--       par '$'
--       par '.'
--       par ','
--       par '#'
--       par 'p'
--       par 'g'
--       par ' '
--       par '@'
--   in par now

lowercaseRomanDigits, uppercaseRomanDigits :: [Char]
lowercaseRomanDigits = "ivxlcdm"
uppercaseRomanDigits = map toUpper lowercaseRomanDigits

romanNumber :: Stream s m Char => Bool -> ParsecT s u m Int
romanNumber uppercase = do
  let romans = if uppercase
                 then uppercaseRomanDigits
                 else lowercaseRomanDigits
  let [one, five, ten, fifty, hundred, fivehundred, thousand] = map char romans
  thousands    <- many thousand >>= (return . (*1000) . length)
  ninehundreds <- option 0 $ try (hundred >> thousand >> return 900)
  fivehundreds <- many fivehundred >>= (return . (*500) . length)
  fourhundreds <- option 0 $ try (hundred >> fivehundred >> return 400)
  hundreds     <- many hundred >>= (return . (*100) . length)
  nineties     <- option 0 $ try (ten >> hundred >> return 90)
  fifties      <- many fifty >>= (return . (*50) . length)
  fourties     <- option 0 $ try (ten >> fifty >> return 40)
  tens         <- many ten >>= (return . (*10) . length)
  nines        <- option 0 $ try (one >> ten >> return 9)
  fives        <- many five >>= (return . (*5) . length)
  fours        <- option 0 $ try (one >> five >> return 4)
  ones         <- many one >>= (return . (*1) . length)
  let total = thousands + ninehundreds + fivehundreds + fourhundreds + hundreds + nineties
            + fifties + fourties + tens + nines + fives + fours + ones
  return total

solution :: String -> Int
solution str = case parse (romanNumber True) "" str of
  Right num -> num
  Left err -> error (show err)

off :: Integer -> [Integer]
off n = 
  let table = replicate (fromIntegral n) 1
      change 0 = 1
      change 1 = 0
      switch table i = map (\(a, ind) -> if mod ind i == 0 then change a else a) $ zip table [1..]
      table' = foldl switch table [1..n]
  in map snd . filter ( (==0) . fst ) $ zip table' [1..]

digit5 :: String -> Int
digit5 str = 
  let digits = map (\r -> read [r]) str :: [Int]
      m = maximum (drop 4 . reverse $ digits)
      init [_, _, _, _] = []
      init [_, _, _] = []
      init [_, _] = []
      init [_] = []
      init [] =[]
      init (n : xs) | n == m = take 4 xs : init xs
                    | otherwise = init xs
      (a1, b1) = change (init digits)
      (a2, b2) = change b1
      (a3, b3) = change b2
      (a4, b4) = change b3
  in m*10000 + a1*1000 + a2*100 + a3*10 + a4

change :: [[Int]] -> (Int, [[Int]])
change digitList = 
  let out = map head digitList
      head (a:b) = (a, b)
      m = maximum . map fst $ out
  in (m, map snd . filter ((==m) . fst) $ out)

isMerge :: String -> String -> String -> Bool
-- isMerge obj part1 part2 = elem obj (merge part1 part2)
isMerge (o1:o2) str1@(s1:r1) str2@(s2:r2)
  | o1 == s1 && o1 /= s2 = isMerge o2 r1 str2
  | o1 /= s1 && o1 == s2 = isMerge o2 str1 r2
  | o1 == s1 && o1 == s2 = isMerge o2 r1 str2 || isMerge o2 str1 r2
  | otherwise = False
isMerge [] [] [] = True
isMerge a [] b = a == b
isMerge a b [] = a == b
isMerge _ _ _ = False

merge :: String -> String -> [String]
merge [] s2 = [s2]
merge s1 [] = [s1]
merge str1@(s1:r1) str2@(s2:r2) =
  map (s1:) (merge r1 str2) ++
  map (s2:) (merge str1 r2)


parseData :: Stream s m Char => ParsecT s u m (String, [Double])  
-- "Rome: Jan 81.2,..." -> ("Rome", [81.2,...])
parseData = do
  name <- manyTill (satisfy isAlpha) (char ':')
  months <- sepBy1 (count 4 anyChar >> many (satisfy isDigit <|> char '.') >>= return . read ) (char ',')
  return (name, months)

mean :: String -> String -> Double
mean city input = 
  case parse (sepBy1 parseData (char '\n')) "" input of
    Right sourceData -> case lookup city sourceData of
                          Just val -> mean' val
                          Nothing  -> -1
    Left err -> error $ show err

variance :: String -> String -> Double
variance city input = 
  case parse (sepBy1 parseData (char '\n')) "" input of
    Right sourceData -> case lookup city sourceData of
                          Just val -> variance' val
                          Nothing  -> -1
    Left err -> error $ show err

mean' :: (Num a, Fractional a) => [a] -> a
mean' xs = sum xs / (fromIntegral . length $ xs)

variance' :: (Num a, Fractional a) => [a] -> a
variance' xs = 
  let m = mean' xs
      n = fromIntegral $ length xs
  in (/n) . sum . map ((^2) . (subtract m)) $ xs

data0 :: String
data0 = "Rome:Jan 81.2,Feb 63.2,Mar 70.3,Apr 55.7,May 53.0,Jun 36.4,Jul 17.5,Aug 27.5,Sep 60.9,Oct 117.7,Nov 111.0,Dec 97.9\n\
\London:Jan 48.0,Feb 38.9,Mar 39.9,Apr 42.2,May 47.3,Jun 52.1,Jul 59.5,Aug 57.2,Sep 55.4,Oct 62.0,Nov 59.0,Dec 52.9\n\
\Paris:Jan 182.3,Feb 120.6,Mar 158.1,Apr 204.9,May 323.1,Jun 300.5,Jul 236.8,Aug 192.9,Sep 66.3,Oct 63.3,Nov 83.2,Dec 154.7\n\
\NY:Jan 108.7,Feb 101.8,Mar 131.9,Apr 93.5,May 98.8,Jun 93.6,Jul 102.2,Aug 131.8,Sep 92.0,Oct 82.3,Nov 107.8,Dec 94.2\n\
\Vancouver:Jan 145.7,Feb 121.4,Mar 102.3,Apr 69.2,May 55.8,Jun 47.1,Jul 31.3,Aug 37.0,Sep 59.6,Oct 116.3,Nov 154.6,Dec 171.5\n\
\Sydney:Jan 103.4,Feb 111.0,Mar 131.3,Apr 129.7,May 123.0,Jun 129.2,Jul 102.8,Aug 80.3,Sep 69.3,Oct 82.6,Nov 81.4,Dec 78.2\n\
\Bangkok:Jan 10.6,Feb 28.2,Mar 30.7,Apr 71.8,May 189.4,Jun 151.7,Jul 158.2,Aug 187.0,Sep 319.9,Oct 230.8,Nov 57.3,Dec 9.4\n\
\Tokyo:Jan 49.9,Feb 71.5,Mar 106.4,Apr 129.2,May 144.0,Jun 176.0,Jul 135.6,Aug 148.5,Sep 216.4,Oct 194.1,Nov 95.6,Dec 54.4\n\
\Beijing:Jan 3.9,Feb 4.7,Mar 8.2,Apr 18.4,May 33.0,Jun 78.1,Jul 224.3,Aug 170.0,Sep 58.4,Oct 18.0,Nov 9.3,Dec 2.7\n\
\Lima:Jan 1.2,Feb 0.9,Mar 0.7,Apr 0.4,May 0.6,Jun 1.8,Jul 4.4,Aug 3.1,Sep 3.3,Oct 1.7,Nov 0.5,Dec 0.7"

data1 :: String
data1 = "Rome:Jan 90.2,Feb 73.2,Mar 80.3,Apr 55.7,May 53.0,Jun 36.4,Jul 17.5,Aug 27.5,Sep 60.9,Oct 147.7,Nov 121.0,Dec 97.9\n\
\London:Jan 58.0,Feb 38.9,Mar 49.9,Apr 42.2,May 67.3,Jun 52.1,Jul 59.5,Aug 77.2,Sep 55.4,Oct 62.0,Nov 69.0,Dec 52.9\n\
\Paris:Jan 182.3,Feb 120.6,Mar 188.1,Apr 204.9,May 323.1,Jun 350.5,Jul 336.8,Aug 192.9,Sep 66.3,Oct 63.3,Nov 83.2,Dec 154.7\n\
\NY:Jan 128.7,Feb 121.8,Mar 151.9,Apr 93.5,May 98.8,Jun 93.6,Jul 142.2,Aug 131.8,Sep 92.0,Oct 82.3,Nov 107.8,Dec 94.2\n\
\Vancouver:Jan 155.7,Feb 121.4,Mar 132.3,Apr 69.2,May 85.8,Jun 47.1,Jul 31.3,Aug 37.0,Sep 69.6,Oct 116.3,Nov 154.6,Dec 171.5\n\
\Sydney:Jan 123.4,Feb 111.0,Mar 151.3,Apr 129.7,May 123.0,Jun 159.2,Jul 102.8,Aug 90.3,Sep 69.3,Oct 82.6,Nov 81.4,Dec 78.2\n\
\Bangkok:Jan 20.6,Feb 28.2,Mar 40.7,Apr 81.8,May 189.4,Jun 151.7,Jul 198.2,Aug 197.0,Sep 319.9,Oct 230.8,Nov 57.3,Dec 9.4\n\
\Tokyo:Jan 59.9,Feb 81.5,Mar 106.4,Apr 139.2,May 144.0,Jun 186.0,Jul 155.6,Aug 148.5,Sep 216.4,Oct 194.1,Nov 95.6,Dec 54.4\n\
\Beijing:Jan 13.9,Feb 14.7,Mar 18.2,Apr 18.4,May 43.0,Jun 88.1,Jul 224.3,Aug 170.0,Sep 58.4,Oct 38.0,Nov 19.3,Dec 2.7\n\
\Lima:Jan 11.2,Feb 10.9,Mar 10.7,Apr 10.4,May 10.6,Jun 11.8,Jul 14.4,Aug 13.1,Sep 23.3,Oct 1.7,Nov 0.5,Dec 10.7" 

anagrams :: String -> [String] -> [String]
anagrams w = filter ((== sort w) . sort)

palindromeChainLength :: Integer -> Integer
palindromeChainLength n = 
  let out = takeWhile (not . ispalindrome) (iterate step n)
      step m = m + (read . reverse . show $ m) 
  in fromIntegral . length $ out

ispalindrome :: Integer -> Bool
ispalindrome n = n == (read . reverse . show $ n)

scramble :: [Char] -> [Char] -> Bool
-- scramble s1 s2 = 
--   let n   = length s2
--       s2' = sort s2
--   in s2' `elem` combination n (sort s1)
scramble s1 s2 = scramble' (sort s2) (sort s1)

scramble' :: [Char] -> [Char] -> Bool
scramble' str1@(s1:r1) (s2:r2)
  | s1 == s2 = scramble' r1 r2
  | otherwise = scramble' str1 r2
scramble' (_:_) [] = False
scramble' []  _  = True

combination :: Int -> [a] -> [[a]]
combination _ [] = []
combination k list@(l1:l2)
  | k == 0 = [[]]
  | otherwise = case drop (k-1) list of
                  [] -> []
                  [_] -> [list]
                  _ -> map (l1:) (combination (k-1) l2) ++
                       combination k l2

yourOrderPlease :: String -> String
yourOrderPlease input = 
  let input' = words input 
      pos :: [Int]
      pos = map (read . filter isDigit) input'
  in unwords . map snd . sort . zip pos $ input'

base64 :: [(Char, Integer)]
base64 = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" [0..]

base64ToBase10 :: String -> Integer
base64ToBase10 str =
  let 
      base = map (64^) [0..]
      num = reverse . map changeTo $ str
      changeTo s = case lookup s base64 of
        Just a -> a
        Nothing -> error "illegal base64number"
  in sum $ zipWith (*) num base

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just (TreeNode left right val)) = 
  val : mergeListByTree (treeByLevels left) (treeByLevels right)
mergeListByTree :: [a] -> [a] -> [a]
mergeListByTree list1 list2 = 
  step list1 list2 1
  where step [] li _ = li
        step li [] _ = li
        step l1 l2 n = 
          let (a1, b1) = splitAt n l1
              (a2, b2) = splitAt n l2
          in a1 ++ a2 ++ step b1 b2 (2*n)

longestSlideDown :: [[Int]] -> Int
longestSlideDown list = undefined


fix :: (a -> a) -> a
fix f = let x = f x in x

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' rev [a] = [a]
reverse' rev (a:as) = rev as ++ [a]

foldr' :: ((a->b->b)->b->[a]->b) -> (a->b->b) -> b -> [a] -> b
foldr' foldhelp f b []     = b
foldr' foldhelp f b (a:as) = f a (foldhelp f b as)

inc, dec, equ:: Integer -> Integer -> Integer
inc k start | k == 0 = 1
            | k > 0  = (sum . map (inc (k-1)) $ [start..9])
dec k start | k == 0 = 1
            | k > 0  = (sum . map (dec (k-1)) $ [0..start])
equ k start = 1
totalIncDec :: Integer -> Integer
totalIncDec k | k == 0 = 1
              | k == 1 = 10
              | otherwise = sum (map (inc (k-1)) [0..9]) +
                            sum (map (dec (k-1)) [0..9]) -
                            10

fib :: Integer -> Integer
fib n | n < 0 = - (fib (-n))
      | otherwise = fibs !! (fromIntegral n)
      where fibs = 0:1:zipWith (+) fibs (tail fibs)

spiralize :: Int -> [[Int]]
spiralize 2 = [[1, 1], [0, 1]]
spiralize 3 = [[1, 1, 1], [0, 0, 1], [1, 1, 1]]
spiralize 4 = [[1,1,1,1], [0,0,0,1], [1,0,0,1], [1,1,1,1]]
spiralize 5 = [[1,1,1,1,1], [0,0,0,0,1], [1,1,1,0,1], [1,0,0,0,1], [1,1,1,1,1]]
spiralize n =
  let first : rest = spiralize (n-4)
  in (replicate n 1) :
     (reverse . (1 : ) . replicate (n-1) $ 0) :
     ([1, 1] ++ first ++ [0, 1]) :
     [1 : 0 : col ++ [0, 1] | col <- rest] ++ 
     [1 : replicate (n-2) 0 ++ [1] , replicate n 1]
showSpial :: [[Int]] -> String
showSpial spi = unlines [[if elem == 0 then '.' else '0' | elem <- row] | row <- spi]


atom :: Stream s m Char => ParsecT s u m (String, Int)
atom = do
  first <- upper
  rest <- many lower
  num <- many digit
  if null num 
    then return (first : rest, 1)
    else return (first : rest, read num)
atomWithParen :: Stream s m Char => ParsecT s u m [(String, Int)]
atomWithParen = do
  open' <- lookAhead (oneOf "([{")
  let (open, close) = case open' of
                        '(' -> ('(', ')')
                        '{' -> ('{', '}')
                        '[' -> ('[', ']')
  inner <- between (char open) (char close) molecule
  num <- many digit
  let num' = if null num then 1 else read num
  return $ fmap (\(a, b) -> (a, b * num')) inner
molecule :: Stream s m Char => ParsecT s u m [(String, Int)]
molecule = fmap (group . concat) $ many1 ((many1 atom) <|> atomWithParen)
  where change li = (fst . head $ li, sum . fmap snd $ li)
        group = fmap change . groupBy ((==) `on` fst) . sort
parseMolecule :: String -> Either String [(String, Int)]
parseMolecule str = 
  case parse molecule "" str of
    Right val -> Right val
    Left err  -> Left "Not a valid molecule"

-- | Roman Numerals Encoders
romanEncoder :: Int -> String
romanEncoder n | n < 0 = error "number should not be less than 0"
romanEncoder n | n > 4000 = error "number should not be greater than 4000"
romanEncoder n = encode n romans
  where
    encode n [(1, s)] = concat (replicate n s)
    encode n ((a, s) : rest) = 
      let (x, y) = quotRem n a
      in if 1 <= x && x <=3
           then concat (replicate x s) ++ encode y rest
           else encode n rest
    romans :: [(Int, String)]
    romans = [ (1000, "M")
             , (900, "CM")
             , (500,  "D")
             , (400, "CD")
             , (100,  "C")
             , (90,  "XC")
             , (50,   "L")
             , (40,  "XL")
             , (10,   "X")
             , (9,   "IX")
             , (5,    "V")
             , (4,   "IV")
             , (1,    "I")]

firstNotNull :: Foldable t => t (Maybe a) -> Maybe a
firstNotNull = getFirst . foldMap First

decompose :: Integer -> Maybe [Integer]
decompose m = 
  let upper = m-1
      lower = 1 -- ceiling . sqrt . fromIntegral $ (2*m) -- a lower bound approximately
      de :: Integer -> Integer -> Maybe [Integer]
      de m n | m == n^2 = Just [n]
      de m n =
        let rest = m - n^2
            n' = min (n-1) . floor . sqrt . fromIntegral $ rest
        in (fmap (n:)) . firstNotNull . map (de rest) $ [n', n'-1 .. 1]
  in firstNotNull . map (de (m^2)) $ [upper, upper-1.. lower]
