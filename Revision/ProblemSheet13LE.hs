module ProblemSheet13LE where
import ProblemSheet12LE
import ProblemSheet11LE
-- 8 f)
is_Final :: Config -> Bool
is_Final (Final _)  = True
is_Final _          = False

-- 8 g)
sos_stm :: Config -> Config
sos_stm (Inter Skip s) = Final s
sos_stm (Inter (Ass n a) s) = Final (update s (a_val a s) n)
sos_stm (Inter (Comp s1 s2) s) | is_Final next = let (Final s') = next in Inter s2 s'
                               | otherwise     = let (Inter s1' s') = next in Inter (Comp s1' s2) s'
                               where next = sos_stm (Inter s1 s)
sos_stm (Inter (If b s1 s2) s) | b_val b s == True = sos_stm (Inter s1 s)
                               | otherwise         = sos_stm (Inter s2 s)
sos_stm (Inter (While b s1) s) = sos_stm (Inter (If b (Comp s1 (While b s1)) Skip) s)


-- 8 h)
deriv_seq :: Config -> [Config]
deriv_seq (Final s) = [Final s]
deriv_seq c         = [c] ++ deriv_seq (sos_stm c)

-- 8 i)
s_sos :: Stm -> State -> State
s_sos stm s = s'
  where
    Final s' = sos_stm (Inter stm s)
