{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
data Free f a = Var a
              | Op (f (Free f a))

class Functor f => Alg f a where
  alg :: f a -> a

eval :: Alg f b => (a -> b) -> Free f a -> b
eval gen (Var x) = gen x
eval gen (Op op) = alg (fmap (eval gen) op)


-- 2
data State s k = Put s k
               | Get (s->k)

instance Functor (State s) where
  fmap f (Put s x) = Put s (f x)
  fmap f (Get x) = Get (f.x)

-- 2.1
get :: Free (State s) s
get = Op (Get Var)

put :: s -> Free (State s) ()
put x = Op (Put x (Var ()))

-- 2.2
instance Alg (State s) (s -> (a,s)) where
  alg (Put s k) _ = k s
  alg (Get k) s = (k s) s

-- 2.3
runState :: Free (State s) a -> s -> (a,s)
runState prog s = eval gen prog s
  where
    gen x s = (x,s)

ex1 = Var 5
ex2 = Op (Put (Var 5) (\x->x+1))
