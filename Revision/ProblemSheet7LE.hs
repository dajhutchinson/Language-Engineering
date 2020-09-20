{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map.Strict as Map
-- 1
data Free f a = Var a
              | Op (f (Free f a))

class Functor f => Alg f a where
  alg :: f a -> a

data AddF a = AddF a a
instance Functor AddF where
  fmap f (AddF x y) = AddF (f x) (f y)

instance Alg AddF Int where
  alg (AddF x y) = x + y
-- 1.1
ex1=Op (AddF (Var "5") (Var "7"))
ex2=Op (AddF (Op (AddF (Var "5") (Var "7"))) (Var "6"))

-- 1.2

eval :: Alg f b => (a -> b) -> Free f a -> b
eval gen (Var x) = gen x
eval gen (Op op) = alg (fmap (eval gen) op)

-- :t x = a
-- :t op = f
-- :t fmap = (a -> b) -> f a -> f b

-- 1.3
gen :: String -> Int
gen "x" = 4
gen "y" = 6
gen _ = 0

-- 1.4
add :: (String->Int)->Free AddF String->Int
add gen t = eval gen t

-- 1.5
add' :: Map.Map String Int -> Free AddF String -> Int
add' m t = eval gen t
  where
    gen x = m Map.!x

-- 1.6
instance Alg AddF [a] where
   alg (AddF x y) = x++y

vars :: Free AddF a -> [a]
vars t = eval gen t
  where
    gen x = [x]


-- 1.7
extract :: Alg f a => Free f a -> a
extract t = eval id t


-- 2.1
data (f:+:g) a = L (f a) | R (g a)
instance (Functor f, Functor g) => Functor (f:+:g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R x) = R (fmap f x)

type Ident = String

type While = Stm :+: Aexp :+: Bexp
data Stm k = Skip | Ident ::= k | k :> k | If k k k | While k k
data Aexp k = Num Int | IdentF Ident | k :+ k | k :* k | k :- k
data Bexp k = T | F | k := k | k :<= k | k :&& k | Not k

instance Functor Stm where
  fmap f Skip = Skip
  fmap f (s ::= k) = s ::= (f k)
  fmap f (x :> y) = (f x) :> (f y)
  fmap f (If x y z) = If (f x) (f y) (f z)
  fmap f (While x y) = While (f x) (f y)

instance Functor Aexp where
  fmap f (Num x) = Num x
  fmap f (IdentF s) = IdentF s
  fmap f (x :+ y) = (f x) :+ (f y)
  fmap f (x :* y) = (f x) :* (f y)
  fmap f (x :- y) = (f x) :- (f y)

instance Functor Bexp where
  fmap f T = T
  fmap f F = F
  fmap f (x := y) = (f x) := (f y)
  fmap f (x :<= y) = (f x) :<= (f y)
  fmap f (x :&& y) = (f x) :&& (f y)
  fmap f (Not x) = Not (f x)

-- 2.2
instance Alg Stm [Ident] where
  alg Skip = []
  alg (s ::= x) = [s]
  alg (x :> y) = x++y
  alg (If x y z)  = x++y++z
  alg (While x y)  = x++y

instance Alg Aexp [Ident] where
  alg (Num n) = []
  alg (IdentF s) = [s]
  alg (x :+ y) = x++y
  alg (x :* y) = x++y
  alg (x :- y) = x++y

instance Alg Bexp [Ident] where
  alg T = []
  alg F = []
  alg (x := y) = x++y
  alg (x :<= y) = x++y
  alg (x :&& y) = x++y
  alg (Not x) = x

varss :: Free While a -> [Ident]
varss = eval (const [])

-- 2.3
instance Alg Stm [Int] where
  alg Skip = []
  alg (s ::= x) = x
  alg (x :> y) = x++y
  alg (If x y z)  = x++y++z
  alg (While x y)  = x++y

instance Alg Aexp [Int] where
  alg (Num n) = [n]
  alg (IdentF s) = []
  alg (x :+ y) = x++y
  alg (x :* y) = x++y
  alg (x :- y) = x++y

instance Alg Bexp [Int] where
  alg T = []
  alg F = []
  alg (x := y) = x++y
  alg (x :<= y) = x++y
  alg (x :&& y) = x++y
  alg (Not x) = x

consts :: Free While a -> [Int]
consts = eval (const [])
