-- 1.1.1
data Robot = Forward Int Robot
          | LeftTurn Robot
          | RightTurn Robot
          | Stop

-- 1.1.2
distTrav :: Robot -> Int
distTrav (Forward n r) = n + distTrav r
distTrav (LeftTurn r)  = distTrav r
distTrav (RightTurn r) = distTrav r
distTrav (Stop)        = 0

ex2 = distTrav((Forward 1 (LeftTurn (RightTurn (Forward 4 Stop)))))

-- 1.1.3
distTravForward :: Int -> Robot -> Int
distTravForward 0 (Forward n r) = n + distTravForward 0 r
distTravForward d (Forward n r) = distTravForward d r
distTravForward d (LeftTurn r) = distTravForward (mod (d-1) 4) r
distTravForward d (RightTurn r) = distTravForward (mod (d+1) 4) r
distTravForward _ (Stop) = 0

distTrav1 :: Robot -> Int
distTrav1 r = distTravForward 0 r

ex3 = distTrav1((Forward 1 (LeftTurn (Forward 4 Stop))))

-- 1.1.4
distTrav2 :: Robot -> Float
distTrav2 r = sqrt(fromIntegral(x*x) + fromIntegral(y*y))
  where
    y = distTravForward 0 r
    x = distTravForward 1 r

ex4 = distTrav2((Forward 3 (LeftTurn (Forward 4 Stop))))

-- 1.2.1
potato=(0, 3, False, "potato")

-- 1.2.2
peel :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
peel (t, w, c, s) = (t+(2*w), w, c, "peeled "++s)

roast :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
roast (t, w, c, s) = (t+70, w, True, "roasted "++s)

boilem :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
boilem (t, w, c, s) = (t+25, w, True, "boiled "++s)

mashem :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
mashem (t, w, c, s) = (t+w, w, c, "mashed "++s)

stickeminastew :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
stickeminastew (t, w, c, s) = (t+120, w, True, "stewed "++s)

-- 1.2.3
mix :: (Int, Int, Bool, String) -> (Int, Int, Bool, String) -> (Int, Int, Bool, String)
mix (t1, w1, False, s1) (t2, w2, _, s2)     = (t1+t2, w1+w2, False, s1++" "++s2)
mix (t1, w1, _, s1)     (t2, w2, False, s2) = (t1+t2, w1+w2, False, s1++" "++s2)
mix (t1, w1, _, s1) (t2, w2, _, s2) = (t1+t2, w1+w2, True, s1++" "++s2)

-- 1.2.4
carrot = (0, 1, False, "carrot")
addCarrot :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
addCarrot (t, w, c, s) = mix (t, w, c, s) carrot

parsnip = (0, 1, False, "carrot")
addParsnip :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
addParsnip (t, w, c, s) = mix (t, w, c, s) parsnip
