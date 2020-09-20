module ProblemSheet12LE where
import ProblemSheet11LE

-- 7 a)
data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         deriving (Show, Eq, Read)

-- 7 b)
p :: Stm
p = Comp
      (Ass
          "z"
          (N 0)
      )
      (While
          (Le
              (V "y")
              (V "x")
          )
          (Comp
              (Ass
                  "z"
                  (Add
                      (V "z")
                      (N 1)
                  )
              )
              (Ass
                  "x"
                  (Sub
                      (V "x")
                      (V "y")
                  )
              )
          )
      )

-- 7 c)
update :: State -> Z -> Var -> State
update s i v y | v == y    = i
               | otherwise = s y

-- 7 d)
s' :: State
s' = update s'' 17 "x"
  where
    s'' = update s 5 "y"

-- 7 e)
data Config = Inter Stm State
            | Final State

-- 7 f)
ns_stm :: Config -> Config
ns_stm (Inter Skip s) = Final s
ns_stm (Inter (Ass n a) s) = Final (update s (a_val a s) n)
ns_stm (Inter (Comp s1 s2) s) = Final s''
  where
    Final s   = ns_stm (Inter s1 s)
    Final s'' = ns_stm (Inter s2 s')
ns_stm (Inter (If b s1 s2) s) | (b_val b s) == True = ns_stm (Inter s1 s)
                              | otherwise           = ns_stm (Inter s2 s)
ns_stm (Inter (While b s1) s) | (b_val b s) == True = ns_stm (Inter (Comp s1 (While b s1)) s)
                              | otherwise           = Final s

-- 7 g)
s_ns :: Stm -> State -> State
s_ns stm s = s'
  where
    Final s' = ns_stm (Inter stm s)
