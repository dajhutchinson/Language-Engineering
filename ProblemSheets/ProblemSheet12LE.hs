module ProblemSheet12LE where

import ProblemSheet11LE

-- 7 (a)
data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
      deriving (Show, Eq, Read)

-- 7 (b)
p :: Stm
p = (Comp
        (Ass "z" (N 0))
        (While
            (Le (V "y") (V "x"))
            (Comp
                (Ass "z" (Add (V "z") (N 1)))
                (Ass "x" (Sub (V "x") (V "y")))
            )
        )
    )

-- 7 (c)
update :: State -> Z -> Var -> State
update s v x y | x==y      = v
               | otherwise = s y

-- 7 (d)
s' :: State
s' = update (update s 17 "x") 5 "y"

-- 7 (e)
data Config = Inter Stm State
            | Final State

-- 7 (f)
ns_stm :: Config -> Config
ns_stm (Inter (Ass x a) s) = Final (update s (a_val a s) x)
ns_stm (Inter Skip s) = Final s
ns_stm (Inter (Comp s1 s2) s) = Final s''
  where
    Final s'  = ns_stm (Inter s1 s)
    Final s'' = ns_stm (Inter s2 s')
ns_stm (Inter (If b s1 s2) s)
  | b_val b s = ns_stm (Inter s1 s)
  | otherwise = ns_stm (Inter s2 s)
ns_stm (Inter (While b s1) s)
  | b_val b s = Final s''
  | otherwise = Final s
  where
    Final s'  = ns_stm (Inter s1 s)
    Final s'' = ns_stm (Inter (While b s1) s')

-- 7 (g)
s_ns :: Stm -> State -> State
s_ns s1 s = s'
  where
    Final s' = ns_stm (Inter s1 s)

-- 7 (h)
ex1 = map (s_ns p s') ["x","y"]
