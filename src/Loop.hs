import Control.Monad
import Control.Monad.State
import qualified Data.Sequence as Seq

data Node a = Node a (Node a) deriving (Read)

instance Show a => Show (Node a) where
  show (Node a (Node b _)) = "node: " ++ show a ++ " -> " ++ show b
instance Eq a => Eq (Node a) where
  Node a _ == Node b _ = a == b
next :: Node a -> Node a
next (Node _ n) = n

loopSize :: Eq a => Node a -> Int
loopSize n0 = 
  let (out, nn:_) = evalState (breakM loopState (iterate next n0)) Seq.empty
      len1 = length (takeWhile (/=nn) out)
  in length out - len1

loopState :: Eq a => Node a -> State (Seq.Seq (Node a)) Bool
loopState n = do
  st <- get
  if n `elem` st 
    then return True
    else put (n Seq.<| st) >> return False

breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM p (x:xs) = do
  b <- p x
  if b 
    then return ([], x:xs)
    else do
      (left, right) <- breakM p xs
      return (x : left, right)
breakM p [] = return ([], [])

node1 = Node 1 node2
node2 = Node 2 node3
node3 = Node 3 node4
node4 = Node 4 node5
node5 = Node 5 node6
node6 = Node 6 node7
node7 = Node 7 node8
node8 = Node 8 node9
node9 = Node 9 node10
node10 = Node 10 node11
node11 = Node 11 node12
node12 = Node 12 node13
node13 = Node 13 node14
node14 = Node 14 node4
ns = [node1, node2, node3, node4]


