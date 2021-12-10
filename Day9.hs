module Day9 where
import Common

type Row = [Int]
type Map = [Row]
type Coord = (Int,Int)

parse = map parseln

parseln :: String -> Row
parseln xs = map chtoi xs

rowcut :: Int -> [Int] -> [Int]
rowcut n xs = out where 
    (out,_) = splitN 3 xrng
    (_, xrng) = splitN (n-1) xs

nsquare :: Coord -> Map -> (Map, Coord)
nsquare (x,y) mp = (sqr, (nx,ny)) where
    sqr = map (rowcut x) ys
    (ys,_) = splitN (2+ny) yfull
    (_,yfull) = splitN (y-1) mp
    ny = if y == 0 then 0 else 1
    nx = if x == 0 then 0 else 1

get :: Map -> Coord -> Maybe Int
get mp (x,y) = if insqr (x,y)
               then Just ((mp !! y) !! x)
               else Nothing where
    (dimx, dimy) = (length (head mp), length mp)
    insqr (a,b) = xinsqr a && yinsqr b
    xinsqr x_ = x_ >= 0 && x_ < dimx
    yinsqr y_ = y_ >= 0 && y_ < dimy

expand :: [Maybe a] -> [a]
expand [] = []
expand (Nothing:xs) = expand xs
expand ((Just x):xs) = x : expand xs

neigh :: Coord -> Map -> [Int]
neigh (x,y) mp = expand mvals where
    mvals = map (get sqr) coords
    coords = [(sx-1,sy), (sx+1,sy), (sx,sy-1), (sx,sy+1)]
    (sqr, (sx,sy)) = nsquare (x,y) mp

islow :: Int -> [Int] -> Bool
islow _ [] = True
islow n (x:xs) = if n < x then islow n xs else False

filterlow :: Map -> [Int]
filterlow mp = map defl low where
    low = filter nlow zp
    zp = [(x,y,n) | (y,row) <- zip [0..] mp, (x,n) <- zip [0..] row]
    nlow = \(x,y,n) -> islow n (neigh (x,y) mp)
    defl = \(x,y,n) -> n

solve :: [String] -> Int
solve xs = sum . (map (+1)) . filterlow $ (parse xs)

solve' :: [String] -> Int
solve' xs = 2

main :: IO ()
main = do
    file <- readFile "input/day9.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
