module Day5 where
import Common

type Map = [[Int]]
type Coord = (Int, Int)
data Line = Diag Coord Coord
          | Vert Coord Int
          | Hori Coord Int
          deriving Show

-- line checks

vert :: Coord -> Coord -> Bool
vert (a,_) (x,_) = a == x

hori :: Coord -> Coord -> Bool
hori (_,b) (_,y) = b == y

-- ctor
newmap :: Int -> Int -> Map
newmap x y = gen y (gen x 0)

newdiag :: Coord -> Coord -> Line
newdiag (a,b) (x,y) | b < y = Diag (a,b) (x,y)
                    | otherwise = Diag (x,y) (a,b)

newvert :: Coord -> Coord -> Line
newvert (x,a) (_,b) = Vert (x, mi) ma where
    (mi, ma) = minmax a b

newhori :: Coord -> Coord -> Line
newhori (a,y) (b,_) = Hori (mi,y) ma where
    (mi, ma) = minmax a b

newline :: Coord -> Coord -> Line
newline a b | vert a b = newvert a b
            | hori a b = newhori a b
            | otherwise = newdiag a b

-- neat necessary functions
-- gen, sign and rgn are refactored to common
-- gen :: Int -> a -> [a]
-- rng :: Int -> Int -> [Int]
-- sign :: Int -> Int

minmax a b = (min a b, max a b)

twosplit :: Int -> Int -> [a] -> ([a],[a],[a])
twosplit a b xs = (start,mid,end) where
    (mid, end) = splitN (ma-mi+1) modend
    (start, modend) = splitN mi xs
    (mi, ma) = minmax a b

-- parsers

parsept :: String -> Coord
parsept x = (a,b) where
    a = atoi . head $ abs
    b = atoi . last $ abs
    abs = wordsWhen (==',') x

parse :: String -> Line
parse x = newline a b where
    a = parsept . head $ wrds
    b = parsept . last $ wrds
    wrds = words x

-- render lines on map. one render per solution

render :: Map -> [Line] -> Map
render = foldl addln

render' :: Map -> [Line] -> Map
render' = foldl addln'

-- separate renders for each case
-- rendering is just adding one to each coordinate in line segment

addln :: Map -> Line -> Map
addln b (Diag _ _) = b
addln b (Vert p m) = addvert b (Vert p m)
addln b (Hori p m) = addhori b (Hori p m)

addln' :: Map -> Line -> Map
addln' mp (Diag a b) = adddiag mp (Diag a b)
addln' mp ln = addln mp ln  -- save a line :)

addvert :: Map -> Line -> Map
addvert mp (Vert (px,py) my) = s ++ modmid ++ e where
    modmid = map (update px px) mid
    (s,mid,e) = twosplit py my mp

addhori :: Map -> Line -> Map
addhori mp (Hori (px,py) mx) = a ++ modf:(tail b) where
    modf = update px mx (head b)
    (a,b) = splitN py mp

adddiag :: Map -> Line -> Map
adddiag mp (Diag (a,b) (x,y)) = s ++ newmid ++ e where
    -- due to how diagonals are parsed: b <= y
    newmid = map adder xonmid
    xonmid = zip (rng a x) mid

    (s,mid,e) = twosplit b y mp

    adder = \(x,row) -> update x x row

-- I did not get clever and so all updates/updates

update :: Int -> Int -> [Int] -> [Int]
update a b xs = s ++ modmid ++ e where
    modmid = map (+1) mid
    (s,mid,e) = twosplit a b xs

-- ensure correct dimensions of board

maxcoord :: Line -> Coord
maxcoord (Diag (a,b) (x,y)) = (max a x, max b y)
maxcoord (Vert (x,y) my) = (x, max y my)
maxcoord (Hori (x,y) mx) = (max x mx, y)

combmaxpt :: Coord -> Coord -> Coord
combmaxpt (a,b) (x,y) = (max a x, max b y)

maxpt :: Coord -> [Line] -> Coord
maxpt mp [] = mp
maxpt mp (x:xs) = maxpt (combmaxpt mp (maxcoord x)) xs

-- solution

slv :: (Map -> [Line] -> Map) -> [String] -> Int
slv rendr xs = sum cnts where
    cnts = map length $ map (filter (>=2)) mp
    mp = rendr nmap lines
    nmap = newmap (maxx+1) (maxy+1)
    (maxx,maxy) = maxpt (0,0) lines
    lines = map parse xs

solve :: [String] -> Int
solve = slv render

solve' :: [String] -> Int
solve' = slv render'

main :: IO ()
main = do
    file <- readFile "input/day5.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
