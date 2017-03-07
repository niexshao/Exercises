{- For Euler Project 96 -}
import Data.Array
import Data.Foldable
import Data.Traversable
import Data.List ((\\), union, sort, intersect, group, sortBy, isPrefixOf, partition)
import Data.Ord (comparing)
import Control.Arrow (second)
import Text.Parsec.String
import Text.Parsec

sudoParser :: Parser (String, Puzzle)
sudoParser = do
  name <- many1 (noneOf "\n\r") <* newline
  mat <- count 9 (count 9 digit <* newline)
  return (name, Puzzle . listArray ((0,0), (8,8)) . map (\n -> read [n] :: Cell) . concat $ mat)

isQualify :: Puzzle -> Bool
isQualify = all (==map Unique [1..9]) . map (sort . snd) . transform 
  
debug = do
  content <- readFile "debug"
  case parse sudoParser "" content of
    Right (name, prob) -> case sudoku prob of
      Just out -> print out
      _        -> putStrLn "not solved" 
  
main = do
  content <- readFile "data"
  let change [Unique a, Unique b, Unique c] = 100*a + 10*b + c
  case parse (many1 sudoParser) "" content of
    Left err -> putStrLn ("parse error :" ++ show err)
    Right problems -> do
      toprighNumbs <- forM problems $ \(name, puz) -> do
        putStrLn $ "for problem: " ++ name ++ " result: "
        case sudoku puz of
          Nothing  -> putStrLn "not legal sudoku problem" >> error "no digit for it"  
          Just out@(Puzzle out') -> print out >> putStrLn ("isQualified:" ++ show (isQualify out)) >> return (map (out' !) [(0,0), (0,1), (0,2)]) 
      putStrLn $ "and the sum of digits is :" ++ show (sum . map change $toprighNumbs)
  
data Cell = Unique Int | Choice [Int] deriving (Eq, Ord)
newtype Puzzle = Puzzle (Array (Int, Int) Cell) deriving (Eq)

isUnique :: Cell -> Bool
isUnique (Unique _) = True
isUnique _          = False


-- 3 Basic Rule
candidateRule :: [Cell] -> [Cell]
candidateRule cs =
  let uni = map (\(Unique n) -> n) . filter isUnique $ cs
      subset (Unique n) = Unique n
      subset (Choice xs') = Choice (xs' \\ uni)
  in map subset cs

placeFindRule :: [Cell] -> [Cell]
placeFindRule cs =  
  let
      (uni', cho') = partition isUnique cs
      uni = map (\(Unique n) -> n) uni'
      cho = (\\ uni) . map head . filter ((==1) . length) . group . sort . concatMap (\(Choice xs) -> xs) $ cho'
      subset (Unique n) = Unique n
      subset xs@(Choice xs') =
        let out = intersect cho xs'
        in case out of
             [o] -> Choice [o]
             []  -> xs 
             _   -> Choice []
  in map subset cs
  
occupyRule :: [Cell] -> [Cell]
occupyRule cs = foldr go cs cs
  where go (Unique _)  xs = xs
        go (Choice ys) xs =
          let sub (Unique _)  = False
              sub (Choice zs) = isPrefixOf (sort zs) ys'
              ys' = sort ys
              subset (Unique n) = Unique n
              subset (Choice zs) = if sub (Choice zs)
                then Choice zs
                else Choice (zs \\ ys)
          in if (== (length ys)). length . filter sub $ cs
               then map subset cs
               else cs     

transform :: Puzzle -> [([(Int, Int)], [Cell])]
transform (Puzzle puz) =
  [ unzip [((i, j), puz ! (i, j)) | i <- [0..8]] | j <- [0..8] ] ++
  [ unzip [((i, j), puz ! (i, j)) | j <- [0..8]] | i <- [0..8] ] ++
  [ unzip [((i, j), puz ! (i, j)) | i <- sets !! col, j <- sets !! row ] | num <- [0..8], let (col, row) = divMod num 3 ]
  where sets = [[0,1,2],[3,4,5],[6,7,8]]
 
cellUnion (Choice xs) (Choice ys) = Choice (xs `intersect` ys)
cellUnion _           y           = y

-- method1 composed by 3 basic rule
method1 :: Puzzle -> Maybe Puzzle
method1 puz =
  let
    splitPuz = transform puz
    candidates = map (second candidateRule) splitPuz
    placeFinds = map (second placeFindRule) splitPuz
    occupies   = map (second occupyRule)    splitPuz
    reverseTrans = concat [ zip pos cells | (pos, cells) <- (candidates ++ placeFinds ++ occupies)]
    out = accumArray cellUnion (Choice [1..9]) ((0, 0), (8, 8)) reverseTrans 
  in case reducePuz out of
       Nothing  -> Nothing
       Just puz' -> if Puzzle puz' == puz then Just puz else method1 (Puzzle puz')

reducePuz :: Ix i => Array i Cell -> Maybe (Array i Cell)
reducePuz puz = forM puz change
  where change (Choice []) = Nothing
        change (Choice [x]) = Just (Unique x)
        change x = Just x

-- method2 force to try-and-failed
method2 :: Puzzle -> Maybe Puzzle
method2 (Puzzle puz) =
  let takeOut = sortBy (comparing (length . (\(Choice xs) -> xs) . snd) ) . filter (not . isUnique . snd) . assocs $ puz
  in case takeOut of
       (pos, Choice ys@(_:_:_)) : _ -> go puz ys
         where go p [] = Nothing
               go p (x:xs) = case sudoku (Puzzle (puz // [(pos, Unique x)])) of
                 Just out -> Just out
                 Nothing  -> go p xs
       []  -> fail "not possible, if has solution"

sudoku :: Puzzle -> Maybe Puzzle
sudoku puz = do
  puz1 <- method1 puz
  if isQualify puz1
    then return puz1
    else do
      puz2 <- method2 puz1
      if isQualify puz2
        then return puz2
        else fail "no solution"
  
isFinished :: Puzzle -> Bool
isFinished (Puzzle puz) = all isUnique puz

instance Show Cell where
  show (Unique n)  = show n
  show (Choice xs) = "X: available number is " ++ unwords (fmap show xs)
instance Read Cell where
  readsPrec _ ('0':ns) = [(Choice [1..9], ns)]
  readsPrec _ (n:ns) = [(Unique (read [n]), ns)]
instance Show Puzzle where
  show (Puzzle puz) = init . unlines . par . fmap change . toList $ puz  
    where change (Unique n)   = head $ show n
          change (Choice _) = 'X'
          par [] = []
          par xs = let (a, b) = splitAt 9 xs
                   in a : par b
