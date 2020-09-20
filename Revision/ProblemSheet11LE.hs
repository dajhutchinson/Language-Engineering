module ProblemSheet11LE where
import Prelude hiding (Num)
import qualified Prelude (Num)

-- 6 a)
type Num = Integer
type Var = String

-- 6 c)
type Z = Integer
type T = Bool

-- 6 d)
type State = Var -> Z

-- 6 e)
data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
          deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
          deriving (Show, Eq, Read)

-- 6 f)
n_val :: Num -> Z
n_val x = id x

-- 6 g)
s :: State
s = f
  where
    f "x" = 1
    f "y" = 2
    f "z" = 3
    f _ = 0

-- 6 h)
a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

-- 6 i)
a_val :: Aexp -> State -> Z
a_val (N n) _ = n
a_val (V v) s = s v
a_val (Mult x y) s = (a_val x s) * (a_val y s)
a_val (Add x y) s = (a_val x s) + (a_val y s)
a_val (Sub x y) s = (a_val x s) - (a_val y s)

-- 6 j)
ex1 = a_val a s

-- 6 k)
b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

-- 6 l)
b_val :: Bexp -> State -> T
b_val TRUE _ = True
b_val FALSE _ = False
b_val (Neg b) s = not (b_val b s)
b_val (And b c) s = (b_val b s) && (b_val c s)
b_val (Eq x y) s = (a_val x s) == (a_val y s)
b_val (Le x y) s = (a_val x s) <= (a_val y s)

-- 6 m)
ex2 = b_val b s
