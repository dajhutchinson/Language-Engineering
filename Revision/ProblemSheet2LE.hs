import Data.List
import qualified Data.Map.Strict as Map
-- 1.2.1
data Graph = Empty
           | Vertex Int
           | Overlay Graph Graph
           | Connect Graph Graph
           deriving Show

g1 = Connect (Overlay (Vertex 2) (Vertex 3)) (Overlay (Overlay (Vertex 2) (Vertex 6)) (Vertex 4))

-- 1.2.2
vertices :: Graph -> [Int]
vertices Empty = []
vertices (Vertex n) = [n]
vertices (Overlay g1 g2) = nub ((vertices g1) ++ (vertices g2))
vertices (Connect g1 g2) = nub ((vertices g1) ++ (vertices g2))

ex1 = vertices g1 -- [2,3,4,6]

-- 1.2.3
edges :: Graph -> Int
edges Empty = 0
edges (Vertex n) = 0
edges (Overlay g1 g2) = (edges g1) + (edges g2)
edges (Connect g1 g2) = (length (vertices g1)) * (length (vertices g2)) + (edges g1) + (edges g2)

ex2 = edges g1 -- 6

-- 1.2.4
roots :: Graph -> [Int]
roots Empty = []
roots (Vertex n) = [n]
roots (Overlay g1 g2) = nub((roots g1)++(roots g2))
roots (Connect g1 g2) = (roots g1) \\ (vertices g2) -- roots of g1 less those that are vertices in g2

ex3 = roots g1 -- [3]

-- 1.3.1
type VGraph = [Int]
vEmpty :: VGraph
vVertex :: Int -> VGraph
vOverlay :: VGraph -> VGraph -> VGraph
vConnect :: VGraph -> VGraph -> VGraph

vEmpty = []
vVertex n = [n]
vOverlay g1 g2 = nub (g1 ++ g2)
vConnect g1 g2 = nub (g1 ++ g2)

ex4 = vConnect (vConnect (vVertex 2) (vVertex 3)) (vOverlay (vOverlay (vVertex 2) (vVertex 6)) (vVertex 4))

-- 1.3.2
type EGraph = (Int,[Int]) -- Edges, vertices

eEmpty :: EGraph
eVertex :: Int -> EGraph
eOverlay :: EGraph -> EGraph -> EGraph
eConnect :: EGraph -> EGraph -> EGraph

eEmpty         = (0,[])
eVertex n      = (0,[n])
eOverlay g1 g2 = ((fst g1)+(fst g2)
               ,  nub((snd g1) ++ (snd g2)))
eConnect g1 g2 = ((length (snd g1) * length (snd g2)) + (fst g1) + (fst g2)
               ,  nub((snd g1) ++ (snd g2)))

ex5 = eConnect (eConnect (eVertex 2) (eVertex 3)) (eOverlay (eOverlay (eVertex 2) (eVertex 6)) (eVertex 4))

--1.4.1
class Graphy graph where
  empty :: graph
  vertex :: Int -> graph
  overlay :: graph -> graph -> graph
  connect :: graph -> graph -> graph

newtype Vertices = Vertices [Int]
instance Graphy Vertices where
  empty                               = Vertices []
  vertex n                            = Vertices [n]
  overlay (Vertices v1) (Vertices v2) = Vertices (nub (v1++v2))
  connect (Vertices v1) (Vertices v2) = Vertices (nub (v1++v2))

newtype Edges = Edges (Int,[Int])
instance Graphy Edges where
  empty                         = Edges (0,[])
  vertex n                      = Edges (0,[n])
  overlay (Edges g1) (Edges g2) = Edges ((fst g1)+(fst g2)
                                , nub((snd g1) ++ (snd g2)))
  connect (Edges g1) (Edges g2) = Edges ((length (snd g1) * length (snd g2)) + (fst g1) + (fst g2)
                                , nub((snd g1) ++ (snd g2)))

-- 1.4.2
instance Graphy Graph where
  empty = Empty
  vertex n = Vertex n
  overlay g1 g2 = Overlay g1 g2
  connect g1 g2 = Connect g1 g2

-- 1.5.1
ring :: Graphy graph => [Int] -> graph
ring xs = foldr overlay empty (zipWith connect vs vs')
  where
    vs = map vertex xs
    vs' = last vs:init vs

-- 1.5.2 a)
-- Add to Graph | Ring :: [Int]
-- vertices (Ring xs) = xs
-- edges (Ring xs) = length xs

-- 1.7.1
lists :: Graph -> Map.Map Int [Int]
lists Empty = Map.empty
lists (Vertex n) = Map.singleton n []
lists (Overlay g1 g2) = Map.union (lists g1) (lists g2)
lists (Connect g1 g2) = foldr (Map.adjust (++target)) g origin
  where
    g = lists (overlay g1 g2)
    origin = vertices g1
    target = vertices g2

-- 1.7.2
newtype AdjacencyList = AdjacencyList (Map.Map Int [Int])
instance Graphy AdjacencyList where
  empty = AdjacencyList (Map.empty)
  vertex n = AdjacencyList (Map.singleton n [])
  overlay (AdjacencyList m1) (AdjacencyList m2) = AdjacencyList (Map.union m1 m2)
  connect (AdjacencyList m1) (AdjacencyList m2) = AdjacencyList m
   where
     m = foldr (Map.adjust (++target)) g origin
     (AdjacencyList g) = overlay (AdjacencyList m1) (AdjacencyList m2)
     origin = Map.keys m1
     target = Map.keys m2
