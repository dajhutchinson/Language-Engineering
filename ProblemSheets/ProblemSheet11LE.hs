module ProblemSheet11LE where

import Prelude hiding (Num) -- removes conflict with Num in prelude
import qualified Prelude (Num)

-- While syntactic categories synonyms
type Num=Integer
type Var=String

-- While sematic types synonyms
type Z=Integer
type T=Bool
type State=Var->Z

-- While syntactic classes
data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add  Aexp Aexp
          | Sub  Aexp Aexp
          deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq  Aexp Aexp
          | Le  Aexp Aexp
          deriving (Show, Eq, Read)

-- 6 f)
n_val :: Num -> Z
n_val 0 = 0
n_val 1 = 1
n_val n = (mod n 10)+2*(n_val (quot n 10))

-- 6 g)
s::State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

-- 6 h)
a::Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

-- 6 i)
a_val :: Aexp -> State -> Z
a_val (N n) _        = n_val(n)
a_val (V v) s        = s v
a_val (Add  a1 a2) s = (a_val a1 s) + (a_val a2 s)
a_val (Mult a1 a2) s = (a_val a1 s) * (a_val a2 s)
a_val (Sub  a1 a2) s = (a_val a1 s) - (a_val a2 s)

-- 6 k)
b::Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

-- 6 l)
b_val :: Bexp -> State -> T
b_val TRUE  _ = True
b_val FALSE _ = False
b_val (Neg b) s = not (b_val b s)
b_val (And b1 b2) s = (b_val b1 s) && (b_val b2 s)
b_val (Eq  a1 a2) s = (a_val a1 s) == (a_val a2 s)
b_val (Le  a1 a2) s = (a_val a1 s) <= (a_val a2 s)
