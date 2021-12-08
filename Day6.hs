module Day6 where
import Common

-- each value corresponds to the number of fish of that index
type Vec = [Int]

-- convert occurrences of valid numbers to occurrence vector
-- example input -> output pairs:
--      [0]          -> [1, 0, 0, 0, 0, 0, 0, 0, 0]
--      [8]          -> [0, 0, 0, 0, 0, 0, 0, 0, 1]
--      [1, 2, 3, 3] -> [0, 1, 1, 2, 0, 0, 0, 0, 0]
newvec :: [Int] -> Vec
newvec xs = foldl sum' h t  where
    (h,t) = (head units, tail units)
    units = map unitvec xs
    
-- unit vector
-- example i -> o
--      0 -> [1, 0, 0, 0, 0, 0, 0, 0, 0]
--      2 -> [0, 0, 1, 0, 0, 0, 0, 0, 0]
--      8 -> [0, 0, 0, 0, 0, 0, 0, 0, 1]
unitvec :: Int -> Vec
unitvec n = modifyN n 1 zero where
    zero = replicate 9 0

-- vector sum
sum' :: Vec -> Vec -> Vec
sum' a b = map esum (zip a b) where
    esum = \(k,l) -> k+l

-- parse
parse :: String -> [Int]
parse xs = (map atoi) . wordsWhen (==',') $ xs

-- step simulation once
step :: Vec -> Vec
step xs =  sum' zvec shifted where 
    zvec = map (*zunit) zaction
    shifted = tail xs ++ [0]

    zunit = head xs
    zaction = sum' (unitvec 6) (unitvec 8)

-- step simulation n times
multistep :: [Int] -> Int -> [Int]
multistep xs 0 = xs
multistep xs n = multistep (step xs) (n-1)

slv :: [Int] -> Int -> Int
slv xs = sum . (multistep (newvec xs)) 

solve :: String -> Int
solve xs = slv (parse xs) 80

solve' :: String -> Int
solve' xs = slv (parse xs) 256

main :: IO ()
main = do
    file <- readFile "input/day6.txt"
    print $ solve file
    print $ solve' file

test :: [Int]
test = [3,4,3,1,2]
