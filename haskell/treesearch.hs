{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-name-shadowing #-}

module Treesearch where
import Test.HUnit (test, assertEqual, runTestTT) -- (~:), 
import qualified Data.Set as Set -- .Strict ?
import qualified Data.Map.Strict as Map
import Data.List
import Data.Function (on)
import Data.Ord (comparing)

-- unwrap (Just v) = v
-- unwrap Nothing = error "unwrap"

second (_:b:_) = b
second _ = error "second: list too short"

-- does that exist already?
segregateBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
segregateBy access l =
  map (\v -> (access (head v), v))
      (groupBy (on (==) access)
               (sortBy (comparing access) l))

-- *Treesearch> segregateBy head [ "hello", "there" ]
-- [('h',["hello"]),('t',["there"])]
-- *Treesearch> segregateBy head [ "hello", "there", "hello" ]
-- [('h',["hello","hello"]),('t',["there"])]
-- *Treesearch> segregateBy head [ "hello", "there", "hi" ]
-- [('h',["hello","hi"]),('t',["there"])]


-- Nodes (vertices, points)

data Node = Node String
  deriving (Show, Eq, Ord)

nodeName (Node n) = n


-- Edges (links, path segments, lines)

data Edge = Edge Node Node Double
  deriving (Show, Eq, Ord)

edgeFrom (Edge a _ _)= a
edgeTo (Edge _ b _)= b
edgeDistance (Edge _ _ d) = d
edgeReverse (Edge a b d) = Edge b a d


-- Paths

data Path = Path [Edge] Double
  deriving (Show, Eq)

instance Ord Path where
  compare = on compare pathDistance

path edges = Path (reverse edges) (sum $ map edgeDistance edges)

--pathEdges (Path es _) = es
pathDistance (Path _ d) = d

pathAdd (Path edges totdist) edge =
  Path (edge:edges) (totdist + (edgeDistance edge))

pathHead (Path (s:_) _) = s
pathHead _ = error "pathHead: empty path"

-- a 'nicer show'
pathView :: Path -> (Double, [String])
pathView (Path ss d) =
  (d, if ((length l) >= 2) && ((head l) == (second l)) then
        tail l
      else
        l)
  where
    l = map (nodeName . edgeFrom) $ (reverse ss) ++ [(Edge to0 to0 0)]
    (Edge _ to0 _) = head ss


-- Frontier: the collection of paths already taken.

data Frontier = Frontier (Set.Set Path)
  deriving (Show, Eq)

frontier paths = Frontier (Set.fromList paths)

frontierNull :: Frontier -> Bool
frontierNull (Frontier p) = Set.null p

frontierRemoveChoice :: Frontier -> Maybe (Path, Frontier)
frontierRemoveChoice (Frontier ps) =
  Set.minView ps >>= \(p, ps') -> Just (p, Frontier ps')

frontierAdd :: Frontier -> Path -> Frontier
frontierAdd (Frontier ps) p = Frontier $ Set.insert p ps


frontierUpdate :: Frontier -> Set.Set Node -> Path -> [Edge] -> Frontier
frontierUpdate front visited path edges =
  foldl' (\ front edge ->
           if Set.member (edgeTo edge) visited then
             front
           else
             frontierAdd front (pathAdd path edge))
         front
         edges


treesearch :: [Edge] -> Node -> Node -> Maybe Path
treesearch edges start end =
  search (frontier [path [Edge start start 0]])
         Set.empty
  where
    search :: Frontier -> Set.Set Node -> Maybe Path
    search front visited =
      case frontierRemoveChoice front of
        Just (path, front') ->
          let node = edgeTo $ pathHead path in
            if node == end then
              Just path
            else
              search (frontierUpdate front' visited path (edgesFor node))
                     (Set.insert node visited)
        Nothing -> Nothing

    edgesFor :: Node -> [Edge]
    edgesFor =
      let m= Map.fromList $ segregateBy edgeFrom
                                        (edges ++ (map edgeReverse edges))
      in
        \ node -> case Map.lookup node m of
                    Just v -> v
                    Nothing -> []



ts :: [Edge] -> String -> String -> Maybe (Double, [String])
ts ss a b =
  (treesearch ss (Node a) (Node b)) >>= (Just . pathView)

es = [(Edge (Node "Arad") (Node "Zerind") 75),
      (Edge (Node "Arad") (Node "Sibiu") 140),
      (Edge (Node "Arad") (Node "Timisoara") 118),
      (Edge (Node "Zerind") (Node "Dradea") 71),
      (Edge (Node "Dradea") (Node "Sibiu") 151),
      (Edge (Node "Sibiu") (Node "Fagaras") 99),
      (Edge (Node "Sibiu") (Node "RimnicuVilcea") 80),
      (Edge (Node "Fagaras") (Node "Bucharest") 211),
      (Edge (Node "RimnicuVilcea") (Node "Pitesti") 97),
      (Edge (Node "RimnicuVilcea") (Node "Craiova") 146),
      (Edge (Node "Pitesti") (Node "Craiova") 138),
      (Edge (Node "Pitesti") (Node "Bucharest") 101),
      (Edge (Node "Timisoara") (Node "Lugoj") 111),
      (Edge (Node "Lugoj") (Node "Mehadia") 70),
      (Edge (Node "Mehadia") (Node "Drobeta") 75),
      (Edge (Node "Drobeta") (Node "Craiova") 120),
      (Edge (Node "Bucharest") (Node "Giurgiu") 90),
      (Edge (Node "Bucharest") (Node "Urziceni") 85),
      (Edge (Node "Urziceni") (Node "Hirsova") 98),
      (Edge (Node "Hirsova") (Node "Eforie") 86),
      (Edge (Node "Urziceni") (Node "Vaslui") 142),
      (Edge (Node "Vaslui") (Node "Iasi") 92),
      (Edge (Node "Iasi") (Node "Neamt") 87)]


e :: String -> String -> Double -> Edge
e a b d = Edge (Node a) (Node b) d


f= frontier [path [e "A" "B" 3],
             path [e "A" "C" 2],
             path [e "A" "D" 2.5]]

tests= test
  [
  -- treesearch.scm top TEST form

    assertEqual "1"
    (pathView (path [e "A" "B" 10]))
    (10.0,["A","B"])

  , assertEqual "2"
    (pathView (path [e "A" "B" 10, e "B" "C" 4.5]))
    (14.5,["A","B","C"])


  -- treesearch.scm middle TEST form

  , let Just (p, _f')= frontierRemoveChoice f in
      (assertEqual "p" p (path [e "A" "C" 2]))
  , let Just (_p, f')= frontierRemoveChoice f in
      (assertEqual "f'" f'
        (frontier [ path [e "A" "B" 3]
                  , path [e "A" "D" 2.5]]))

  -- treesearch.scm bottom TEST form

  , assertEqual ""
    (ts [] "A" "B")
    Nothing
  , assertEqual ""
    (ts [e "A" "C" 3] "A" "B")
    Nothing
  , assertEqual ""
    (ts [e "C" "B" 3] "A" "B")
    Nothing
  , assertEqual ""
    (ts [e "X" "Y" 3] "A" "B")
    Nothing
  , assertEqual ""
    (ts [e "A" "A" 3] "A" "B")
    Nothing
  , assertEqual ""
    (ts [e "A" "B" 3] "A" "B")
    (Just (3.0,["A","B"]))
  , assertEqual ""
    (ts [e "B" "A" 3] "A" "B")
    (Just (3.0,["A","B"]))

  --;; multiple direct paths
  , assertEqual ""
    (ts [e "B" "A" 3, e "B" "A" 2] "A" "B")
    (Just (2,["A", "B"]))
  , assertEqual ""
    (ts [e "B" "A" 3, e "A" "B" 2] "A" "B")
    (Just (2,["A", "B"]))
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "A" 2] "A" "B")
    (Just (2,["A", "B"]))
  , assertEqual ""
    (ts [e "A" "B" 3, e "A" "B" 2] "A" "B")
    (Just (2,["A", "B"]))

  --;; unused paths
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2] "A" "B")
    (Just (3, ["A", "B"]))
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2] "B" "A")
    (Just (3, ["B", "A"]))

  -- ;; multi-segment
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2] "A" "C")
    (Just (5, ["A", "B", "C"]))
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2] "C" "A")
    (Just (5, ["C", "B", "A"]))

  -- ;; with alternatives: same length
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2, e "A" "C" 5] "A" "C")
    (Just (5, ["A", "B", "C"]))
  , assertEqual ""
    (ts [e "A" "B" 3, e "B" "C" 2, e "A" "C" 5] "C" "A")
    (Just (5, ["C", "B", "A"]))

    , assertEqual ""
    (ts [e "A" "C" 5, e "A" "B" 3, e "B" "C" 2] "A" "C")
    (Just (5, ["A", "B", "C"]))
    -- ;; it prefers to take the shorter path first. right?
    
    -- ;; with alternative of shorter length
  , assertEqual ""
    (ts [e "A" "C" 4, e "A" "B" 3, e "B" "C" 2] "A" "C")
    (Just (4, ["A", "C"]))
  , assertEqual ""
    (ts [e "A" "C" 4, e "A" "B" 3, e "B" "C" 2] "C" "A")
    (Just (4, ["C", "A"]))


  -- main.scm

  , assertEqual "main 1"
    (ts es "Arad" "Bucharest")
    (Just (418,["Arad","Sibiu","RimnicuVilcea","Pitesti","Bucharest"]))

  , assertEqual "main 1 backwards"
    (ts es "Bucharest" "Arad")
    (Just (418,["Bucharest","Pitesti","RimnicuVilcea","Sibiu","Arad"]))

  
  , assertEqual ""
    (ts es "Lugoj" "Sibiu")
    (Just (369,["Lugoj", "Timisoara", "Arad", "Sibiu"]))
  , assertEqual ""
    (ts es "Mehadia" "Sibiu")
    (Just (421,["Mehadia", "Drobeta", "Craiova", "RimnicuVilcea", "Sibiu"]))
  , assertEqual ""
    (ts es "Mehadia" "Fagaras")
    (Just (520,["Mehadia", "Drobeta", "Craiova", "RimnicuVilcea", "Sibiu",
                "Fagaras"]))
  , assertEqual ""
    (ts es "Drobeta" "Neamt")
    (Just (765,["Drobeta", "Craiova", "Pitesti", "Bucharest", "Urziceni",
                "Vaslui", "Iasi", "Neamt"]))
  ];

t = runTestTT tests

