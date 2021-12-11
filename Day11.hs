module Day11 where
import Common
import Grid

type Map = Grid Int
type FlashMap = Grid (Int,Int)

parseln :: String -> [Int]
parseln xs = map chtoi xs

parse = map parseln

explosion = (>9)

-- first action is to convert the Map to a FlashMap
init' :: Map -> Coord -> Int -> (Int, Int)
init' mp c v = (v, 0)

energize :: FlashMap -> Coord -> (Int,Int) -> (Int, Int)
energize mp c (v,flsh) = (v+1, flsh)

-- absorb surrounding flashes (if zero we have flashed and should be inert)
absorb :: FlashMap -> Coord -> (Int,Int) -> (Int, Int)
absorb mp c (v,flsh) = if v == 0
                       then (0, flsh)
                       else (v+flashes, flsh) where
    flashes = length . filter explosion . map fst . neighbours nrim c $ mp

-- collapse flashed states and increase amount of flashes
collapse :: FlashMap -> Coord -> (Int,Int) -> (Int, Int)
collapse mp c (v,flsh) = if explosion v
                         then (0, flsh+1)
                         else (v, flsh)

value :: FlashMap -> Coord -> (Int,Int) -> Int
value mp c (v,_) = v

flash :: FlashMap -> Coord -> (Int,Int) -> Int
flash mp c (v,_) = v

starter :: Map -> FlashMap
starter = gridapply init'

flashstep :: FlashMap -> FlashMap
flashstep = gridapply collapse . gridapply absorb

exploding :: FlashMap -> Bool
exploding = (>0) . foldr (+) 0 . map (length . filter explosion) . valuegrid

multiflash :: FlashMap -> FlashMap
multiflash mp = if exploding mp
                then multiflash $ flashstep mp
                else mp

step :: FlashMap -> FlashMap
step = multiflash . gridapply energize

multistep :: Int -> FlashMap -> FlashMap
multistep 0 mp = mp
multistep n mp = multistep (n-1) (step mp)

valuegrid :: FlashMap -> Map
valuegrid = gridapply value

flashgrid :: FlashMap -> Map
flashgrid = gridapply flash

flashes :: FlashMap -> Int
flashes = sum . map sum . flashgrid

-- solution
solve :: [String] -> Int
solve xs = 1

solve' :: [String] -> Int
solve' xs = 2

main :: IO ()
main = do
    file <- readFile "input/day10.txt"
    let l = lines file
    print $ solve l
    print $ solve' l

test :: IO Map
test = do
    file <- readFile "input/test11.txt"
    let l = lines file
    return (parse l)
