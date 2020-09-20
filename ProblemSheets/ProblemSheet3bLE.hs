{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

data Fix f = In (f (Fix f))

--1.2
data (f :+: g) a = L (f a)
                 | R (g a)
infixr 5:+:

-- 1.2.1
instance (Functor f, Functor g) => Functor (f:+:g)
  where
    fmap f (L x) = L (fmap f x)
    fmap f (R y) = R (fmap f y)

-- 1.2.2 a)
data ValF k = ValF k
data AddF k = AddF k k

-- 1.2.2 b)
instance Functor ValF
  where
    fmap f (ValF n) = ValF (f n)

instance Functor AddF
  where
    fmap f (AddF x y) = AddF (f x) (f y)

--1.2.3
data SubF k = SubF k k

instance Functor SubF
  where
    fmap f (SubF x y) = SubF (f x) (f y)

-- 1.2.4
(\\) :: (f a -> a) -> (g a -> a) -> (f :+: g) a -> a
(falg \\ galg) (L x) = falg x
(falg \\ galg) (R x) = galg x


val :: ValF Int -> Int
val (ValF x) = x

add :: AddF Int -> Int
add (AddF x y) = x + y

sub :: SubF Int -> Int
sub (SubF x y) = x - y

-- NOTE from notes
evalAdd :: Fix (ValF :+: AddF) -> Int
evalAdd = cata (val \\ add)

evalAddSub :: Fix (ValF :+: AddF :+: SubF) -> Int
evalAddSub = cata (val \\ (add \\ sub))

-- 1.2.5
-- Have to explicitly define function to run for each data type, lots of brackets

-- 1.3
class Functor f => Alg f a
  where
    alg :: f a -> a

-- 1.3.1
instance Alg ValF Int
  where
    -- alg :: ValF Int -> Int
    alg (ValF x) = x

-- 1.3.2
instance Alg AddF Int
  where
    alg (AddF x y) = x + y

-- 1.3.3
instance Alg SubF Int
  where
    alg (SubF x y) = x - y

-- 1.3.4
instance (Alg f a, Alg g a) => Alg (f :+: g) a
  where
    -- alg :: (f :+: g) a -> a
    alg (L x) = alg x
    alg (R y) = alg y

-- 1.3.5
evalAddSub' :: Fix (ValF :+: AddF :+: SubF) -> Int
evalAddSub' = cata alg

-- 1.3.6
cati :: Alg f a => Fix f -> a
cati (In x) = alg (fmap cati x)

-- 1.3.7 a)
data MulF k = MulF k k

instance Functor MulF
  where
    fmap f (MulF x y) = MulF (f x) (f y)

mul :: MulF Int -> Int
mul (MulF x y) = x * y

-- 1.3.7 b)
instance Alg MulF Int
  where
    alg (MulF x y) = x * y

-- 1.3.7 c)
type ExprF = Fix (ValF :+: AddF :+: MulF :+: SubF)

-- 1.3.8 d)
eval :: ExprF -> Int
eval = cati -- NOTE cati wants Fix
