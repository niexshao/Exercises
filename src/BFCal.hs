module BFCal () where

import Control.Monad.State
import Control.Monad.Identity
import System.Random (StdGen, random, randomR, Random, newStdGen)
import qualified Data.Array as Arr
import Data.Array (Array)
import Data.Char (ord, chr, intToDigit)

data Direction = ToL | ToR | ToU | ToD deriving (Bounded, Enum)
instance Random Direction where
  random g = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) g of
               (r, g') -> (toEnum r, g')
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (r, g') -> (toEnum r, g')
move :: (Int, Int) -> Direction -> (Int, Int)
move (a, b) ToD = (a+1, b)
move (a, b) ToU = (a-1, b)
move (a, b) ToR = (a, b+1)
move (a, b) ToL = (a, b-1)

data Config = Config { configArray :: Array (Int, Int) Char
                     , configPosition :: (Int, Int)
                     , configStack :: [Int]
                     , configDirection :: Direction
                     , configStdGen :: StdGen
                     , configOutput :: [Char]}

slice :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Direction -> [(Int, Int)]
slice ((minCol, minRow), (maxCol, maxRow)) (col, row) dir = 
  case dir of
    ToR -> [(col, row') | row' <- [row+1..maxRow]]
    ToL -> [(col, row') | row' <- [row-1,row-2..minRow]]
    ToD -> [(col', row) | col' <- [col+1..maxCol]]
    ToU -> [(col', row) | col' <- [col-1,col-2..minCol]]

interpret :: StdGen -> String -> String
interpret gen str = 
  let
    strArray = toArray str
    config = Config strArray (0,0) [] ToR gen []
  in runIdentity (evalStateT befunge config)

toArray :: String -> Array (Int, Int) Char
toArray str = 
  let 
    str' = lines str
    colnum = length str' - 1
    rownum = (maximum . map length $ str') -1 
  in Arr.array ((0,0), (colnum, rownum)) [((i,j), if j > len then ' ' else (str' !! i !! j)) | i <- [0..colnum], j <- [0..rownum], let len = length (str' !! i) - 1]

befunge :: StateT Config Identity String
befunge = do
  config@(Config strArray positon@(col, row) stack dir gen output) <- get
  let now = strArray Arr.! positon
  case now of
    '!' -> if head stack == 0
             then put (config { configPosition = move positon dir, configStack = 1 : tail stack }) >> befunge
             else put (config { configPosition = move positon dir, configStack = 0 : tail stack }) >> befunge
    '?' -> let (dir', gen') = random gen
           in put (config {configPosition = move positon dir, configStdGen = gen', configDirection = dir'}) >> befunge
    '_' -> if head stack == 0
             then put (config { configPosition = move positon ToR, configDirection = ToR, configStack = tail stack }) >> befunge
             else put (config { configPosition = move positon ToL, configDirection = ToL, configStack = tail stack }) >> befunge
    '|' -> if head stack == 0
             then put (config { configPosition = move positon ToD, configDirection = ToD, configStack = tail stack }) >> befunge
             else put (config { configPosition = move positon ToU, configDirection = ToU, configStack = tail stack }) >> befunge
    '"' -> let
             indices = slice (Arr.bounds strArray) positon dir
             line = map ord . takeWhile (/= '"') $ (map (strArray Arr.!) indices)
             newPosition = foldr (\_ acc -> move acc dir) positon line
           in put (config {configPosition = move (move newPosition dir) dir, configStack = reverse line ++ stack }) >> befunge
    ':' -> if null stack 
             then put (config { configPosition = move positon dir, configStack = [0] }) >> befunge
             else put (config { configPosition = move positon dir, configStack = head stack:stack }) >> befunge 
    '\\' -> case stack of
             [] -> error "at least one value to swap"
             [val] -> put (config { configPosition = move positon dir, configStack = [0, val]}) >> befunge
             val1:val2:rem -> put (config { configPosition = move positon dir, configStack = val2:val1:rem}) >> befunge
    '$' -> put (config { configPosition = move positon dir, configStack = tail stack}) 
           >> befunge
    '.' -> put (config { configPosition = move positon dir, configStack = tail stack, configOutput = intToDigit (head stack) : output}) >> befunge
    ',' -> put (config { configPosition = move positon dir, configStack = tail stack, configOutput = chr (head stack) : output }) >> befunge
    '#' -> put (config { configPosition = move (move positon dir) dir}) >> befunge
    'p' -> let y:x:v:rem = stack
               newArray = strArray Arr.// [((x, y), chr v)]
           in put (config { configArray = newArray, configPosition = move positon dir, configStack = rem}) >> befunge
    'g' -> let y:x:rem = stack
               val = ord $ strArray Arr.! (x, y)
           in put (config { configPosition = move positon dir, configStack = val : rem}) >> befunge
    '@' -> return (reverse output)
    ' ' -> put (config { configPosition = move positon dir}) >> befunge
    _ -> if now `elem` "0123456789" 
           then put (config { configPosition = move positon dir, configStack = read [now] : stack}) >> befunge
           else if now `elem` "+-*/%`"
                  then let v1:v2:rem = stack
                       in case now of
                         '+' -> put (config {configPosition = move positon dir, configStack = v1+v2 : rem}) >> befunge
                         '-' -> put (config {configPosition = move positon dir, configStack = v2-v1 : rem}) >> befunge
                         '*' -> put (config {configPosition = move positon dir, configStack = v2*v1 : rem}) >> befunge
                         '/' -> put (config {configPosition = move positon dir, configStack = if v1 == 0 then 0 : rem else div v2 v1 : rem}) >> befunge
                         '%' -> put (config {configPosition = move positon dir, configStack = if v1 == 0 then 0 : rem else mod v2 v1 : rem}) >> befunge
                         '`' -> put (config {configPosition = move positon dir, configStack = if v2 > v1 then 1 : rem else 0 : rem}) >> befunge
                  else if now `elem` "><^v"
                         then case now of
                                '>' -> put (config {configPosition = move positon ToR, configDirection = ToR}) >> befunge
                                '<' -> put (config {configPosition = move positon ToL, configDirection = ToL}) >> befunge
                                '^' -> put (config {configPosition = move positon ToU, configDirection = ToU}) >> befunge
                                'v' -> put (config {configPosition = move positon ToD, configDirection = ToD}) >> befunge
                         else error "Uncongnized Character"

test :: String -> IO ()
test str = do
  g <- newStdGen
  print $ interpret g str
