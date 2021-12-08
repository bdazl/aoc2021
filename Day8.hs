module Day8 where
import Data.List
import Common

findidx :: Char -> [String] -> Int
findidx c xs = case fnd of
                  Just n -> n
                  Nothing -> error "bad parse"
               where
    fnd = findIndex (c `elem`) xs

parse :: String -> ([String],[String])
parse xs = (sig, tail dout)  where
    (sig,dout) = splitN delim wrds
    delim = findidx '|' wrds
    wrds = words xs

known :: String -> Maybe Int
known xs = case length xs of
              2 -> Just 1
              3 -> Just 7
              4 -> Just 4
              7 -> Just 8
              _ -> Nothing


exact :: String -> Int
exact "abcdeg" = 0
exact "ab"     = 1
exact "acdfg"  = 2
exact "abcdf"  = 3
exact "abef"   = 4
exact "bcdef"  = 5
exact "bcdefg" = 6
exact "abd"    = 7
exact "abcdefg"= 8
exact "abcdef" = 9
exact _ = -1

known' :: String -> Int
known' xs = exact (sort xs)

countn :: Int -> [Maybe Int] -> Int
countn n xs = sum (map pred xs) where
    pred :: Maybe Int -> Int
    pred Nothing = 0
    pred (Just m) = if n == m then 1 else 0

countknown :: [String] -> [Int]
countknown xs = [countn i kn | i <- [0..9]] where
    kn = map known xs

countknown' :: [String] -> [Int]
countknown' xs = [count (==i) kn | i <- [0..9]] where
    kn = map known' xs

rawknown :: ([String] -> [Int]) -> String -> [Int]
rawknown cnt xs = cnt $ snd (parse xs)

sum' :: [Int] -> [Int] -> [Int]
sum' a b = map esum (zip a b) where
    esum = \(k,l) -> k+l

sumall :: [[Int]] -> [Int]
sumall xs = foldl sum' (head xs) (tail xs)

solve :: [String] -> Int
solve xs = sum . sumall . (map (rawknown countknown)) $ xs

solve' :: [String] -> Int
solve' xs = sum . sumall . (map (rawknown countknown')) $ xs

main :: IO ()
main = do
    file <- readFile "input/test8.txt"
    let l = lines file
    print $ solve l
    print $ solve' l

test :: String
test = "cedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
test' = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
