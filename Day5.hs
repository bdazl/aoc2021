module Day4 where
import Common

type Map = [[Int]]
type Coord = (Int, Int)
data Line = Diag Coord Coord
          | Vert Coord Int
          | Hori Coord Int
          deriving Show

vert :: Coord -> Coord -> Bool
vert (a,_) (x,_) = a == x

hori :: Coord -> Coord -> Bool
hori (_,b) (_,y) = b == y

newdiag :: Coord -> Coord -> Line
newdiag (a,b) (x,y) | b < y = Diag (a,b) (x,y)
                    | otherwise = Diag (x,y) (a,b)

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
parseln a b | vert a b = parselnv a b
            | hori a b = parselnh a b
            | otherwise = newdiag a b

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

sign :: Int -> Int
sign a | a < 0 = -1
       | otherwise = 1

rng :: Int -> Int -> [Int]
rng a b | a == b = [a]
        | otherwise = a : rng (a+(sign (b-a))) b

adddiag :: Map -> Line -> Map
adddiag mp (Diag (a,b) (x,y)) = s ++ newmid ++ e where
    -- due to how diagonals are parsed: b <= y
    newmid = map adder xonmid
    xonmid = zip (rng a x) mid

    (s,mid,e) = twosplit b y mp

    adder = \(x,row) -> addrow x x row

addln :: Map -> Line -> Map
addln b (Diag _ _) = b
addln b (Vert p m) = addvert b (Vert p m)
addln b (Hori p m) = addhori b (Hori p m)

addln' :: Map -> Line -> Map
addln' mp (Diag a b) = adddiag mp (Diag a b)
addln' mp ln = addln mp ln

fill :: Map -> [Line] -> Map
fill = foldl addln

fill' :: Map -> [Line] -> Map
fill' = foldl addln'

maxcoord :: Line -> Coord
maxcoord (Diag (a,b) (x,y)) = (max a x, max b y)
maxcoord (Vert (x,y) my) = (x, max y my)
maxcoord (Hori (x,y) mx) = (max x mx, y)

combmaxpt :: Coord -> Coord -> Coord
combmaxpt (a,b) (x,y) = (max a x, max b y)

maxpt :: Coord -> [Line] -> Coord
maxpt mp [] = mp
maxpt mp (x:xs) = maxpt (combmaxpt mp (maxcoord x)) xs


slv :: (Map -> [Line] -> Map) -> [String] -> Int
slv filler xs = sum cnts where
    cnts = map length reduce
    reduce = map (filter (>=2)) mp
    mp = filler (empty (maxx+1) (maxy+1)) lines
    (maxx,maxy) = maxpt (0,0) lines
    lines = map parse xs


solve :: [String] -> Int
solve = slv fill

solve' :: [String] -> Int
solve' = slv fill'

main :: IO ()
main = do
    file <- readFile "input/day5.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
