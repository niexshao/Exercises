{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}
module Imperative where

import Control.Monad.Free

-- thoughts:
-- use a sequence to store the value, because the binding value
-- can be changed repeatly, it must be tracked
-- Var will return the variable index, it won't change, unless the variable is rebinded
-- (+=) will return a Value
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Sequence as Seq
type Imp a = StateT (Seq.Seq Integer) Identity a
data Var = Var Int | Value Integer
type Value = Either Integer Var

def :: Imp Value -> Integer
def st = case runIdentity (runStateT st Seq.empty) of
  (Right (Var ind), env) -> Seq.index env ind
  (Left out, _) -> out

var :: Integer -> Imp Value
var n = do
  env <- get
  put (env Seq.|> n)
  return$ Right (Var (length env))
lit :: Integer -> Value
lit = Left

binary :: (Integer -> Integer -> Integer) -> Value -> Value -> Imp ()
binary f (Right (Var a)) right = do
  env <- get 
  let new = case right of 
              Right (Var b ) -> Seq.index env a `f` Seq.index env b
              Left b -> Seq.index env a `f` b
  put $ Seq.adjust (const new) a env

(+=), (*=), (-=) :: Value -> Value -> Imp ()
(+=) = binary (+)
(*=) = binary (*)
(-=) = binary (-)

while :: Value -> (Integer -> Bool) -> Imp () -> Imp ()
while var@(Right (Var ind)) cond action = do
  env <- get
  let variable = Seq.index env ind
  if cond variable 
    then action >> while var cond action
    else return ()               

main :: Integer
main = def $ do
  a <- var 1
  b <- var 2
  a += b
  a += lit 1
  return a 

factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result


-- copy from `https://gist.github.com/gatlin/9696088`
-- use free monad to create imperative way of evaluation
newtype Then k = Then k
deriving instance Functor Then
type Imperative = Free Then

imperative :: Imperative a -> a
imperative (Free (Then next)) = imperative next
imperative (Pure v)           = v

set :: a -> Imperative a
set = return

mySquare :: Num a => a -> Imperative a
mySquare n = return $ n * n
myAdd :: Num a => a -> a -> Imperative a
myAdd n m = return $ n + m

ex1 :: Int
ex1 = imperative $ do
  a <- set 5
  b <- set 100
  c <- myAdd a b
  return c
