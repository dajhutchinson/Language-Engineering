import Data.List

-- 1.2.1
data Graph = Empty
  | Vertex Int
  | Overlay Graph Graph
  | Connect Graph Graph

-- 1.2.2
-- Connect(Overlay(Vertex 2)(Vertex 3)) (Overlay (Overlay (Vertex 2) (Vertex 6)) (Vertex 4))
vertices :: Graph -> [Int]
vertices (Empty)         = []
vertices (Vertex n)      = [n]
vertices (Overlay g1 g2) = nub ((vertices g1) ++ (vertices g2))
vertices (Connect g1 g2) = nub ((vertices g1) ++ (vertices g2))

-- 1.2.3
edges :: Graph -> Int
edges (Empty)         = 0
edges (Vertex n)      = 0
edges (Overlay g1 g2) = (edges g1) + (edges g2)
edges (Connect g1 g2) = (edges g1) + (edges g2) + (length(vertices g1) * length(vertices g2))

-- 1.2.4
roots :: Graph -> [Int]
roots (Empty)         = []
roots (Vertex n)      = [n]
roots (Overlay g1 g2) = nub (removeAll (intersect (vertices g1) (vertices g2)) (nub ((roots g1) ++ (roots g2))))++(intersect (roots g1) (roots g2))-- (roots g1) + (roots g2) - (g1 intersect g2) + (roots g1 intersect g2)
roots (Connect g1 g2) = removeAll (intersect (vertices g1) (vertices g2)) (roots g1) -- less vertices in g1 & g2

-- removes given element from list
remove :: Int -> [Int] -> [Int]
remove i [] = []
remove i (x:xs)
  | i == x    = remove i xs
  | otherwise = x : (remove i xs)

-- remove list of elements from list
removeAll :: [Int] -> [Int] -> [Int]
removeAll [] ys = ys
removeAll (x:xs) ys = removeAll xs (remove x ys)

-- 1.3.1
type Vertices = [Int]
type Graph' = Vertices

empty :: Graph'
empty = []

vertex :: Int -> Graph'
vertex n = [n]

overlay :: Graph' -> Graph' -> Graph'
overlay g1 g2 = nub (g1++g2)

connect :: Graph' -> Graph' -> Graph'
connect g1 g2 = nub (g1++g2)

-- 1.3.2
type Edges = Int
type Graph'' = (Edges, Vertices)

empty' :: Graph''
empty' = (0,[])

vertex' :: Int -> Graph''
vertex' n = (0, [n])

overlay' :: Graph'' -> Graph'' -> Graph''
overlay' (e1, v1) (e2,v2) = (e1+e2, nub (v1++v2))

connect' :: Graph'' -> Graph'' -> Graph''
connect' (e1, v1) (e2, v2) = (e1+e2+(length v1 * length v2), nub(v1++v2))

-- 1.3.3
-- Doesn't have to recalculate vertices of a graph multiple times
