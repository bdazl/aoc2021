module Day7 where
import Common

parse :: String -> [Int]
parse xs = (map atoi) . wordsWhen (==',') $ xs

fuelcost :: Int -> Int -> Int
fuelcost newpos oldpos = abs (newpos - oldpos)

fuelcost' :: Int -> Int -> Int
fuelcost' npos opos = natsum $ fuelcost npos opos

-- natsum n = 1 + 2 + 3 + ... + n
natsum :: Int -> Int
natsum n = div (n*(n+1)) 2

price :: (Int -> Int -> Int) -> [Int] -> Int -> Int
price cost xs pos = sum . (map (cost pos)) $ xs

range :: [Int] -> (Int,Int)
range xs = (minimum xs, maximum xs)

-- fst output = price (so that we can use order of tuples)
-- snd output = pos
candidates :: (Int->Int->Int) -> [Int] -> [(Int,Int)]
candidates cost xs = zip prange atb  where
    prange = map (price cost xs) atb
    atb = [a..b]
    (a,b) = range xs

findmin :: [Int] -> (Int,Int)
findmin xs = minimum $ candidates fuelcost xs

findmin' :: [Int] -> (Int,Int)
findmin' xs = minimum $ candidates fuelcost' xs

solve :: String -> Int
solve xs = fst . findmin $ (parse xs)

solve' :: String -> Int
solve' xs = fst . findmin' $ (parse xs)

main :: IO ()
main = do
    file <- readFile "input/day7.txt"
    print $ solve file
    print $ solve' file

test :: [Int]
test = [16,1,2,0,4,2,7,1,2,14]
