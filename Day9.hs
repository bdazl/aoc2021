module Day9 where
import Common
import Grid

type Map = Grid Int

parse = map parseln

parseln :: String -> [Int]
parseln xs = map chtoi xs

islow :: Int -> [Int] -> Bool
islow _ [] = True
islow n (x:xs) = if n < x then islow n xs else False

filterlow :: Map -> [Int]
filterlow mp = map defl low where
    low = filter nlow zp
    zp = [(x,y,n) | (y,row) <- zip [0..] mp, (x,n) <- zip [0..] row]
    nlow = \(x,y,n) -> islow n $ neighbours nplus (x,y) mp
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
