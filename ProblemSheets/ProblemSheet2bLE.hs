import Data.List
import Data.Map as Map

-- 1.2.1
data Graph = Empty
  | Vertex Int
  | Overlay Graph Graph
  | Connect Graph Graph
  deriving (Show)

-- 1.4.1
class Graphy a where
  empty  :: a
  vertex  :: Int -> a
  overlay  :: a -> a -> a
  connect  :: a -> a -> a

newtype Vertices = Vertices [Int]
  deriving (Show)

instance Graphy Vertices where
  empty = Vertices []
  vertex x = Vertices [x]
  overlay (Vertices g1) (Vertices g2) = Vertices (nub (g1++g2))
  connect (Vertices g1) (Vertices g2) = Vertices (nub (g1++g2))

newtype Edges = Edges (Int, [Int])
  deriving (Show)

instance Graphy Edges where
  empty = Edges (0,[])
  vertex x = Edges (0,[x])
  overlay (Edges (e1,v1)) (Edges (e2,v2)) = Edges (e1+e2, nub(v1++v2))
  connect (Edges (e1,v1)) (Edges (e2,v2)) = Edges (e1+e2+(length v1 * length v2), nub(v1++v2))

-- 1.4.2
instance Graphy Graph where
  empty = Empty
  vertex x = Vertex x
  overlay g1 g2 = Overlay g1 g2
  connect g1 g2 = Connect g1 g2

-- 1.5.1
-- TODO
--ring :: Graphy graph => [Int] -> graph
--ring [] = Empty

-- 1.7.1
lists :: Graph -> Map Int [Int]
lists Empty = Map.empty
lists (Vertex n) = Map.singleton n []
lists (Overlay g1 g2) = Map.union (lists g1) (lists g2)
lists (Connect g1 g2) = Map.map (nub) (Map.union (Map.map (++ (Map.keys (lists g2))) (lists g1)) (lists g2))
-- Remove dupes (append list of g2 keys to values of g1)

-- 1.7.2
--NOTE what data structure for Graph (In shallow we have done [Int] & (Int, [Int]))
-- I presume (Int, [Int]) but this isn't list adjacent

-- 1.7.3
-- TODO

-- 1.7.4
--NOTE [Int] for vertice names
-- n=len list, a=nxn [[Int]]
-- Aij = 1 if i connects to j
type MatGraph = ([Int], [[Int]])

-- Treates dupes as seperate vertices
mat :: Graph -> MatGraph
mat Empty = ([], [])
mat (Vertex n) = ([n], [[0]])
mat (Overlay g1 g2) = (v1++v2, (overlay' (length e1) e1 (length e2) e2))
  where
    v1 = fst (mat g1)
    v2 = fst (mat g2)
    e1 = snd (mat g1)
    e2 = snd (mat g2)
mat (Connect g1 g2) = (v1++v2, connect' (length e1) e1 (length e2) e2)
  where
    v1 = fst (mat g1)
    v2 = fst (mat g2)
    e1 = snd (mat g1)
    e2 = snd (mat g2)

-- Pads edges with zeroes
--          len(e1)   e1     len(e2)    e2          out
overlay' :: Int -> [[Int]] -> Int -> [[Int]] -> [[Int]]
overlay' _ [] _ [] = []
overlay' n [] m (y:ys) = ((zeroes n)++y) : (overlay' n [] m ys)
overlay' n (x:xs) m ys = (x++(zeroes m)) : (overlay' n xs m ys)

-- Creates list of n zeroes
zeroes :: Int -> [Int]
zeroes 0 = []
zeroes n = 0:(zeroes (n-1))

-- pads edges with 1s
connect' :: Int -> [[Int]] -> Int -> [[Int]] -> [[Int]]
connect' _ [] _ [] = []
connect' n [] m (y:ys) = ((zeroes n)++y) : (overlay' n [] m ys)
connect' n (x:xs) m ys = (x++(ones m)) : (overlay' n xs m ys)

-- creates list of n ones
ones :: Int -> [Int]
ones 0 = []
ones n = 1:(ones (n-1))
