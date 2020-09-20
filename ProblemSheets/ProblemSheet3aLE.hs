data Expr = Val Int
          | Add Expr Expr

-- 1.1.1
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = (eval x) + (eval y)

-- 1.1.2 a)
data Fix f = In (f (Fix f))

inop :: Fix f -> f (Fix f)
inop (In x) = x

-- 1.1.2 b)
data ExprF k = ValF Int
             | AddF k k

-- 1.1.2 c)
ex1 = In(ValF 1)
ex2 = In(ValF 4)
ex3 = In(AddF (In (ValF 1)) (In (ValF 2)))

-- 1.1.2 d)
fromExpr :: Expr -> (Fix ExprF)
fromExpr (Val n) = In (ValF n)
fromExpr (Add x y) = In(AddF (fromExpr x) (fromExpr y))

-- 1.1.3 a)
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

-- 1.1.3 b)
instance Functor ExprF
  where
    fmap f (ValF n) = ValF n
    fmap f (AddF x y) = AddF (f x) (f y)

-- 1.1.3 c)
eval' :: Fix ExprF-> Int
eval' = cata alg
  where
    alg :: ExprF Int -> Int
    alg (ValF n) = n
    alg (AddF x y) = x+y

-- 1.1.3 d)
{- Using cata removes the need for recursion in the definition. TODO expand this -}

-- 1.1.4 a)
toExpr :: Fix ExprF -> Expr
toExpr = cata alg
  where
    alg :: ExprF (Expr) -> Expr
    alg (ValF n) = Val n
    alg (AddF x y) = Add x y

-- 1.1.4 b)
{-
(toExpr . fromExpr) (Val n)
{.}
toExpr (fromExpr (Val n))
{fromExpr}
toExpr (In (ValF n))
{toExpr}
cata alg (In (ValF n))
{cata}
alg (fmap (cata alg) (ValF n))
{fmap}
alg (ValF n)
{alg}
Val n

(toExpr . fromExpr) (Add x y)

(fromExpr . toExpr) (ValF n)

(fromExpr . toExpr) (AddF x y)
-}
