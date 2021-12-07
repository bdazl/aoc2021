module Day6 where
import Common

parse :: String -> [Int]
parse xs = (map atoi) . wordsWhen (==',') $ xs

-- make problem go from 0 -> 6 instead of 6 -> 0

fmod = flip mod
fmod7 = fmod 7
invert n = fmod7 (6-n) 

-- naive (first) approach
step' :: [Int] -> [Int]
step' xs = stepped ++ replicate zeroes 8 where 
    stepped = map stepper xs
    zeroes = count (==0) xs
    stepper = \x -> if x == 0 then 6 else x-1

multistep' :: Int -> [Int] -> [Int]
multistep' 0 xs = xs
multistep' n xs = multistep' (n-1) (step' xs)

slv :: Int -> [Int] -> Int
slv n = length . (multistep' n) 

solve :: String -> Int
solve xs = slv 80 $ parse xs

solve' :: String -> Int
solve' xs = slv 256 $ parse xs

main :: IO ()
main = do
    file <- readFile "input/day6.txt"
    print $ solve file
    print $ solve' file

test :: [Int]
test = [3,4,3,1,2]
