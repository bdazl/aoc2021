module Day12 where
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import Common

------ TYPES

type Row = [Int]
type Mat = [Row]
type Coord = (Int, Int)
type Edge = (Node, Node)
type NodeSet = Set.Set Node

data Graph = G NodeSet Mat  -- undirected graph => symmetric adjecency matrix
data Node = Start | Repeat String | Single String | End
            deriving (Ord, Eq, Show)

-- the state  holds:
--    1. a graph that has been stripped from impossible moves
--    2. the node we are standing on
--    3. the list of previously visited nodes (remove this?)
type State = (Graph, Node, [Node])

------ 

newnode :: String -> Node
newnode xs | xs == "start" = Start
           | xs == "end"   = End
           | allupper xs   = Repeat xs
           | otherwise     = Single xs

allupper :: String -> Bool
allupper = all (isUpper)

nodesete :: [Edge] -> NodeSet
nodesete xs = Set.fromList (fsts ++ snds) where
    snds = map snd xs
    fsts = map fst xs

------  MATRIX

-- matrices are n x n
zeromat :: Int -> Mat
zeromat n = replicate n (replicate n 0)

sum' :: Mat -> Mat -> Mat
sum' ma mb = [vsum a b | (a,b) <- rowps] where
    rowps = zip ma mb
    vsum a b = map psum $ zip a b
    psum (a,b) =  a + b


-- single element set (usually we set two, due to symmetry)
setelem :: Int -> Mat -> Coord -> Mat
setelem v m (x,y) = modifyN y nrow m where
    nrow = modifyN x (clamp' v) row
    row = m !! y

-- set both (a,b) and (b,a)
setsymmetric :: Int -> Mat -> Coord -> Mat
setsymmetric v m (a, b) = sum' ma mb where
    ma = setelem v m (a,b)
    mb = setelem v m (b,a)

setcoords :: Int -> Mat -> [Coord] -> Mat
setcoords v m cs = foldl sum' m mats where
    mats = map (setsymmetric v zm) cs
    zm = zeromat (length m)

-- given a node set and some edges, create an adjacency matrix
edgestomat :: NodeSet -> [Edge] -> Mat
edgestomat ns es = setcoords 1 zm coords where
    coords = foldl (++) [] . map (ecoords ns) $ es
    zm = zeromat (Set.size ns)

clamp' = clamp 0 1

clampmat :: Mat -> Mat
clampmat = map (map clamp')

-------- GRAPH

-- matrix is symmetric, so mostly we'll set two elems of a graph
newgraph :: [Edge] -> Graph
newgraph xs = G (nodesete xs) (newadj xs)

setedge :: Int -> Graph -> Edge -> Graph
setedge v (G ns m) (a,b) = G ns nmat  where
    nmat = setsymmetric v m (ai,bi)
    ai = Set.findIndex a ns
    bi = Set.findIndex b ns

newadj :: [Edge] -> Mat
newadj xs = clampmat mats where 
    mats = setcoords 1 zm coords
    coords = foldl (++) [] coordss
    coordss = map (ecoords nset) xs
    zm = zeromat (Set.size nset)
    nset = nodesete xs


ecoords :: NodeSet -> Edge -> [Coord]
ecoords ns (a,b) = [(ai,bi), (bi,ai)] where
    ai = Set.findIndex a ns
    bi = Set.findIndex b ns


-------- STATE

newstate :: Graph -> State
newstate g = (g, Start, [Start])

candidates :: Graph -> Node -> [Node]
candidates (G ns mat) n = if n == End then [] else nodes where
    nodes = catMaybes . map maybnode . enumerate $ row
    row = mat !! Set.findIndex n ns
    maybnode (i,v) = if v == 1 then Just (Set.elemAt i ns) else Nothing

-- when visiting single/small letter nodes, their connection gets severed
-- by setting the row and column of the adjecency matrix to zero (can't be visited again)
rmcolrow :: Int -> Mat -> Mat
rmcolrow i mat = rm mat where
    rm = transpose . (modifyN i z) . transpose . (modifyN i z)
    z = replicate (length mat) 0

nodeop :: NodeSet -> Mat -> Node -> Mat
nodeop ns mat n = case n of
                    Start -> rmcolrow i mat
                    (Single _) -> rmcolrow i mat
                    (Repeat _) -> mat
                    End -> zeromat (length mat)
                  where
    i = Set.findIndex n ns

stepstate :: State -> Node -> State
stepstate ((G ns mat), old, vs) new = ((G ns nmat), new, vs ++ [old]) where
    nmat = nodeop ns mat old

moves :: State -> [State]
moves ((G ns mat), n, pr) = map (stepstate state) cands where
    cands = candidates (G ns mat) n
    state = ((G ns mat), n, pr)


---------- PARSE

parse :: [String] -> Graph
parse = newgraph . map parseln

parseln :: String -> Edge
parseln xs = (newnode (head wrds), newnode (last wrds)) where
    wrds = wordsWhen (=='-') xs

-- solution

countendpaths :: State -> Int
countendpaths (g, n, pr) | n == End = 1
                         | candidates g n == [] = 0
                         | otherwise = sum cnt where
                         cnt = map countendpaths mvs
                         mvs = moves (g, n, pr)

solve :: [String] -> Int
solve xs = countendpaths $ newstate (parse xs)

solve' :: [String] -> Int
solve' xs = 2

main :: IO ()
main = do
    file <- readFile "input/day12.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
