-- 1.1
data Robot = Fwd Int Robot
           | Lt Robot
           | Rt Robot
           | Stop

-- 1.2
distTrav :: Robot -> Int
distTrav (Fwd n r) = n + (distTrav r)
distTrav (Lt r)    = distTrav r
distTrav (Rt r)    = distTrav r
distTrav (Stop)    = 0

r1 = Fwd 3 (Lt (Rt (Fwd 4 Stop)))
ex1 = distTrav r1 -- 7

-- 1.3
distTravFwd :: Robot -> Int
distTravFwd r = distTravDir 0 r

distTravDir :: Int -> Robot -> Int
distTravDir 0 (Fwd n r) = n + (distTravDir 0 r)
distTravDir d (Fwd n r) = distTravDir d r
distTravDir d (Rt r)    = distTravDir (mod (d+1) 4) r
distTravDir d (Lt r)    = distTravDir (mod (d-1) 4) r
distTravDir _ Stop      = 0

r2 = Fwd 3 (Lt (Fwd 4 Stop))
ex2 = distTravFwd r1 -- 7
ex3 = distTravFwd r2 -- 3

-- 1.4
distTravDiag :: Robot -> Float
distTravDiag r = sqrt (fromIntegral (distTravDir 0 r)^2 + fromIntegral (distTravDir 1 r)^2)

ex4 = distTravDiag r2 -- 3^2 + 4^2 = 5^2
