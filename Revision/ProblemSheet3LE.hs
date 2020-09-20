{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- 1
data Expr = Val Int
          | Add Expr Expr
          deriving Show
-- 1.1
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = (eval x) + (eval y)

ex1 = eval (Add (Add (Val 4) (Val 1)) (Val 3)) -- = 4+1+3 = 8

-- 1.2 a)
data Fix f = In (f (Fix f))
inop :: Fix f -> f (Fix f)
inop (In x) = x

-- 1.2.b)
data ExprF k = ValF Int
             | AddF k k

-- 1.2.c)
type Expr' = Fix ExprF
--In (ValF 5) :: Fix ExprF
--In (AddF (In (ValF 5)) (In (ValF 6))) :: Fix ExprF

-- 1.2.d)
fromExpr :: Expr -> Expr'
fromExpr (Val n) = In (ValF n)
fromExpr (Add x y) = In (AddF (fromExpr x) (fromExpr y))

-- 1.3.a)
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

-- 1.3.b)
instance Functor ExprF where
  fmap f (ValF n) = ValF n
  fmap f (AddF x y) = AddF (f x) (f y)

-- 1.3.c)
eval' :: Expr' -> Int
eval' e' = cata alg e'
  where
    alg (ValF n) = n
    alg (AddF x y) = x + y

ex2 = eval' (In (AddF (In (ValF 5)) (In (ValF 6))))

-- 1.4.a)
toExpr :: Expr' -> Expr
toExpr e' = cata alg e'
  where
    alg (ValF n) = Val n
    alg (AddF x y) = Add x y

ex3 = toExpr (In (AddF (In (ValF 5)) (In (ValF 6))))

-- 2
data (f :+: g) a = L (f a)
                 | R (g a)
infixr 5 :+:

-- 2.1
instance (Functor f, Functor g) => Functor (f:+:g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R x) = R (fmap f x)

-- 2.2.a)
data ValF' k = ValF' Int
data AddF' k = AddF' k k

-- 2.2.b)
instance Functor ValF' where
  fmap f (ValF' n) = ValF' n

instance Functor AddF' where
  fmap f (AddF' x y) = AddF' (f x) (f y)

-- 2.3
data SubF' k = SubF' k k
instance Functor SubF' where
  fmap f (SubF' x y) = SubF' (f x) (f y)

-- 2.4
evalAddSub :: Fix (ValF' :+: AddF' :+: SubF') -> Int
evalAddSub e = cata alg e
  where
    alg (L (ValF' n)) = n
    alg (R (L (AddF' x y))) = x + y
    alg (R (R (SubF' x y))) = x - y

-- 3
class Functor f => Alg f a where
  alg :: f a -> a

-- 3.1
instance Alg ValF' Int where
  alg (ValF' n) = n

-- 3.2
instance Alg AddF' Int where
  alg (AddF' x y) = x + y

-- 3.3
instance Alg SubF' Int where
  alg (SubF' x y) = x - y

-- 3.4
instance (Alg f a, Alg g a) => Alg (f :+: g) a where
  alg (L x) = alg x
  alg (R x) = alg x

-- 3.5
evalAddSub' :: Fix (ValF' :+: AddF' :+: SubF') -> Int
evalAddSub' e' = cata alg e'

-- 3.6
cati :: Alg f a => Fix f -> a
cati e = cata alg e

-- 3.7.a)
data MulF' k = MulF' k k

-- 3.7.b)
instance Functor MulF' where
  fmap f (MulF' x y) = MulF' (f x) (f y)
instance Alg MulF' Int where
  alg (MulF' x y) = x * y

-- 3.7.c)
type ExprM = Fix (ValF' :+: AddF' :+: SubF' :+: MulF')

-- 3.7.d)
evalM :: ExprM -> Int
evalM e = cati e

-- 3.8
newtype Depth = Depth Int

instance Alg ValF' Depth where
  alg (ValF' _) = Depth 1
instance Alg AddF' Depth where
  alg (AddF' (Depth x) (Depth y)) = Depth (1 + max x y)
instance Alg SubF' Depth where
  alg (SubF' (Depth x) (Depth y)) = Depth (1 + max x y)
instance Alg MulF' Depth where
  alg (MulF' (Depth x) (Depth y)) = Depth (1 + max x y)

depth ::  ExprM -> Int
depth e = let (Depth d) = cati e in d
