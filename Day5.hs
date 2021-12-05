module Day4 where
import Common

type Map = [[Int]]
type Coord = (Int, Int)
data Line = Free Coord Coord
          | Vert Coord Int
          | Hori Coord Int
          deriving Show

parsept :: String -> Coord
parsept x = (a,b) where
    a = atoi . head $ abs
    b = atoi . last $ abs
    abs = wordsWhen (==',') x
    
parselnv :: Coord -> Coord -> Line
parselnv (x,a) (_,b) = Vert (x, mi) ma where
    mi = min a b
    ma = max a b

parselnh :: Coord -> Coord -> Line
parselnh (a,y) (b,_) = Hori (mi,y) ma where
    mi = min a b
    ma = max a b

parseln :: Coord -> Coord -> Line
parseln a b | vert (Free a b) = parselnv a b
            | hori (Free a b) = parselnh a b
            | otherwise = Free a b

parse :: String -> Line
parse x = parseln a b where
    a = parsept . head $ wrds
    b = parsept . last $ wrds
    wrds = words x

gen :: Int -> a -> [a]
gen 0 _ = []
gen n x = x:gen (n-1) x

empty :: Int -> Int -> [[Int]]
empty x y = gen y row where
    row = gen x 0

vert :: Line -> Bool
vert (Free (a,_) (x,_)) = a == x
vert (Vert _ _) = True
vert (Hori _ _) = False

hori :: Line -> Bool
hori (Free (_,b) (_,y)) = b == y
hori (Vert _ _) = False
hori (Hori _ _) = True

twosplit :: Int -> Int -> [a] -> ([a],[a],[a])
twosplit a b xs = (start,mid,end) where
    (mid, end) = splitN (ma-mi+1) modend
    (start, modend) = splitN mi xs
    (mi,ma) = (min a b, max a b)

addrow :: Int -> Int -> [Int] -> [Int]
addrow a b xs = s ++ modmid ++ e where
    modmid = map (+1) mid
    (s,mid,e) = twosplit a b xs

addvert :: Map -> Line -> Map
addvert mp (Vert (px,py) my) = s ++ modmid ++ e where
    modmid = map (addrow px px) mid
    (s,mid,e) = twosplit py my mp

addhori :: Map -> Line -> Map
addhori mp (Hori (px,py) mx) = a ++ modf:(tail b) where
    modf = addrow px mx (head b)
    (a,b) = splitN py mp

addln :: Map -> Line -> Map
addln b (Free _ _) = b
addln b (Vert p m) = addvert b (Vert p m)
addln b (Hori p m) = addhori b (Hori p m)

fill :: Map -> [Line] -> Map
fill = foldl addln

maxcoord :: Line -> Coord
maxcoord (Free (a,b) (x,y)) = (max a x, max b y)
maxcoord (Vert (x,y) my) = (x, max y my)
maxcoord (Hori (x,y) mx) = (max x mx, y)

combmaxpt :: Coord -> Coord -> Coord
combmaxpt (a,b) (x,y) = (max a x, max b y)

maxpt :: Coord -> [Line] -> Coord
maxpt mp [] = mp
maxpt mp (x:xs) = maxpt (combmaxpt mp (maxcoord x)) xs

solve :: [String] -> Int
solve xs = sum cnts where
    cnts = map length reduce
    reduce = map (filter (>=2)) mp
    mp = fill (empty (maxx+1) (maxy+1)) lines
    (maxx,maxy) = maxpt (0,0) lines
    lines = map parse xs

solve' :: [String] -> Int
solve' xs = retval where
    retval = 2

main :: IO ()
main = do
    file <- readFile "input/day5.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
