{-# LANGUAGE BangPatterns #-}
-- need a fast way to represent a sudo puzzle
-- use vector reference: https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial
--  * End users should use Data.Vector.Unboxed for most cases
--  * If you need to store more complex structures, use Data.Vector
--  * If you need to pass to C, use Data.Vector.Storable
import qualified Data.Vector as V
import System.Environment
-- import Data.ByteString.Lazy.Char8 as BL
import Data.List (partition, intersect, nub)
  
data Cell = Unique Int | Choice [Int] deriving (Eq)
newtype Puzzle = Puzzle (V.Vector (V.Vector Cell))  
-- for this simple solution, I should just use Array. and make Cell = Int is enough
instance Show Cell where
  show (Unique n)  = show n
  show (Choice _) = "0"
instance Show Puzzle where
  show puz = unlines . map (concatMap show) . puzzleToList $ puz
listToPuzzle :: [[Int]] -> Puzzle
listToPuzzle xs = Puzzle . V.fromList . map change $ xs
  where change = V.fromList . map parse
        parse 0 = Choice [1..9]
        parse n = Unique n
puzzleToList :: Puzzle -> [[Cell]]
puzzleToList (Puzzle puz) = V.toList . (V.map V.toList) $ puz
  
index :: Int -> Int -> Puzzle -> Cell
index i j (Puzzle puz) = (puz V.! i) V.! j
indexCol i puz = [index i j puz | j <- [0..8]]
indexRow j puz = [index i j puz | i <- [0..8]]
indexTri i puz = [index x y puz | x <- sets !! i' , y <- sets !! j']
  where (i', j') = divMod i 3
        sets = [[0,1,2],[3,4,5],[6,7,8]]

isLegal :: Puzzle -> Bool
isLegal puz = all legal [indexCol i puz | i <- [0..8]]
           && all legal [indexRow i puz | i <- [0..8]]
           && all legal [indexTri i puz | i <- [0..8]]
  where legal :: [Cell] -> Bool
        legal xs = 
          let (uni, choices) = partition par xs
              par (Unique _) = True
              par _ = False
              uni' = map (\(Unique x) -> x) uni
              choices' = concatMap (\(Choice ys) -> ys) choices
          in length uni == length (nub uni) -- rest check not used for solution 1 && null (intersect uni' choices')

isFinish :: Puzzle -> Bool
isFinish (Puzzle puz) = V.all id . V.map (V.all par) $ puz
  where par (Unique _) = True
        par _          = False 
  
update :: Puzzle -> ((Int, Int), Cell) -> Puzzle  
update (Puzzle puz) ((m, n), x) = Puzzle (puz V.// [(m, inner)])
  where inner = puz V.! m
        inner' = inner V.// [(n, x)]

solution :: Puzzle -> Maybe Puzzle
solution puz = walk puz 0
  where
    walk puz 81 = if isFinish puz then Just puz else Nothing
    walk puz num =
      let
        (m, n) = divMod num 9 
        cell = index m n puz
        isUnique = case cell of
          Unique _ -> True
          _        -> False 
      in if isUnique
           then walk puz (num+1)
           else go puz m n 1
    go _   _ _ 10 = Nothing
    go puz m n x =
      let out = update puz ((m, n), Unique x)
      in if isLegal out
           then case walk out (m*9+n+1) of
                  Just puz' -> Just puz' 
                  Nothing   -> go puz m n (x+1)
           else go puz m n (x+1)  
parseString :: String -> [[Int]]
parseString = map (map (read . (:[]))) . lines
  
-- main :: IO ()
main = do
  content <- readFile "data"
  let puz = listToPuzzle $ parseString content
  print $ solution puz

-- Why these function alawys give answer to sudoku problem?
-- because it is actually traverse all possible combination for the problem.
-- a brute algorithm, which take a lot of time
