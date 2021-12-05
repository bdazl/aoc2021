module Day4 where
import Common

type Coord = (Int, Int)
type Line = (Coord, Coord)
type Map = [[Int]]

parsept :: String -> Coord
parsept x = (a,b) where
    a = atoi . head $ abs
    b = atoi . last $ abs
    abs = wordsWhen (==',') x
    

parse :: String -> Line
parse x = (a,b) where
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
vert ((a, _), (x, _)) = a == x

horiz :: Line -> Bool
horiz ((_, b), (_, y)) = b == y

addb :: Map -> Coord -> Map
addb b (x,y) = modifyN y nrow b where
    nrow = modifyN x (val+1) row
    val = row !! x
    row = b !! y

fill :: Map -> [Coord] -> Map
fill m = foldl addb m

-- remove me?
fillv :: Map -> Line -> Map
fillv m ((x, a),(_,b)) = fill m pts where
    pts = zip (gen (-1) x) [a..b]

vertpts :: Line -> [Coord]
vertpts ((x,a),(_,b)) = zip (gen (-1) x) [mi..ma] where
    mi = min a b
    ma = max a b

horizpts :: Line -> [Coord]
horizpts ((a,y),(b,_)) = zip [mi..ma] (gen (-1) y) where
    mi = min a b
    ma = max a b

linepts :: Line -> [Coord]
linepts l | vert l = vertpts l
          | horiz l = horizpts l
          | otherwise = []

join :: [[a]] -> [a]
join [] = []
join (x:xss) = x ++ join xss

combmaxpt :: Coord -> Coord -> Coord
combmaxpt (a,b) (x,y) = (max a x, max b y)

maxpt :: Coord -> [Coord] -> Coord
maxpt mp [] = mp
maxpt mp (x:xs) = maxpt (combmaxpt mp x) xs

solve :: [String] -> Int
solve xs = sum cnts where
    cnts = map length reduce
    reduce = map (filter (>=2)) mp
    mp = fill (empty maxx maxy) pts
    (maxx,maxy) = maxpt (0,0) pts
    pts = join . (map linepts) $ lines
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
