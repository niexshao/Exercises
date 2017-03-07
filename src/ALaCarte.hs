{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveFunctor #-}
module ALaCarte () where

data Expr f = In (f (Expr f))

data Lit a = Lit Int
data Add a = Add a a
data Mult a = Mult a a deriving Functor
data Power a = Power a a deriving Functor

type Expression = Expr (Lit :+: Add :+: Mult :+: Power)
-- Coproduct
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 1 :+:

-- By defining functor instances we can write a generic fold operator
-- which will be useful to evaluate our expression
instance Functor Lit where
  fmap f (Lit val) = Lit val

instance Functor Add where
  fmap f (Add a b) = Add (f a) (f b)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl valF) = Inl (fmap f valF)
  fmap f (Inr valR) = Inr (fmap f valR)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In e) = f (fmap (foldExpr f) e)

-- Now we can write a simple interpreter.
class Functor f => Eval f where
  evalAlgebra :: f Int -> Int
instance Eval Lit where
  evalAlgebra (Lit x) = x
instance Eval Add where
  evalAlgebra (Add x y) = x + y
instance Eval Mult where
  evalAlgebra (Mult x y) = x * y
instance Eval Power where
  evalAlgebra (Power x y) = x ^ y
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl valL) = evalAlgebra valL
  evalAlgebra (Inr valR) = evalAlgebra valR

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

-- The problem is that it is painful to write expressions
pain :: Expr (Lit :+: Add)
pain = In (Inr (Add (In (Inl (Lit 5))) (In (Inl (Lit 6)))))

-- Injection 
-- To ease writing expression, we will define a type class
-- which will choose the right constructors for us. Think of sub :<: sup to say that
-- sub is a subtype fof sup
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
-- Reflexivity
instance Functor sub => sub :<: sub where
  inj sub = sub
instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

-- a modern implementation would use type families.
inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj
lit :: (Lit :<: f) => Int -> Expr f
lit n = inject (Lit n)
add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add e1 e2 = inject (Add e1 e2)
mult :: (Mult :<: f) => Expr f -> Expr f -> Expr f
mult e1 e2 = inject (Mult e1 e2)
power :: (Power :<: f) => Expr f -> Expr f -> Expr f
power e1 e2 = inject (Power e1 e2)

-- To add a pretty printer, we define a new type class in much the 
-- same way as for the first intrepreter
class Functor f => Pretty f where
  render :: f String -> String
instance Pretty Lit where
  render (Lit x) = show x
instance Pretty Add where
  render (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
instance Pretty Mult where
  render (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"
instance Pretty Power where
  render (Power x y) = x ++ "^" ++ y
instance (Pretty f, Pretty g) => Pretty (f :+: g) where
  render (Inl valL) = render valL
  render (Inr valR) = render valR

pretty :: Pretty f => Expr f -> String
pretty = foldExpr render

