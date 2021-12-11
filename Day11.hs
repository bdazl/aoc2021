module Day11 where
import Common
import Grid

type ValFlash = (Int, Int)   -- (mine value, flashes)
type MineFlash = (Mine, Int) -- (mine, flashes)

-- three phases of map structures
type Map = Grid Int
type FlashMap = Grid ValFlash
type MineMap = Grid MineFlash

-- to properly handle the fact that mines explodes, the non-exposions (Inac) need to
-- hold information about how many explosions it has absorbed (second argument) as well
-- as the value (first argument)
data Mine = Expl | Inac Int Int deriving (Eq,Show)

newmine :: Int -> Mine
newmine n = if explosion n then Expl else Inac n 0

mabsorb :: Mine -> [Mine] -> Mine
mabsorb Expl _ = Expl
mabsorb (Inac v n) xs = nm where
    nm = if explosion (v+inc) then Expl else Inac (v+inc) expl
    inc = if expl > n then expl-n else 0
    expl = length . filter (==Expl) $ xs

mcollapse :: Mine -> Int -> (Int, Int)
mcollapse Expl flsh       = (0, flsh+1)
mcollapse (Inac v n) flsh = (v, flsh)

parseln :: String -> [Int]
parseln xs = map chtoi xs

parse = map parseln

explosion = (>9)

-- first action is to convert the Map to a FlashMap
init' :: Map -> Coord -> Int -> ValFlash
init' mp c v = (v, 0)

energize :: FlashMap -> Coord -> (Int,Int) -> MineFlash
energize mp c (v,flsh) = (newmine (v+1), flsh)

-- absorb surrounding flashes (if zero we have flashed and should be inert)
absorb :: MineMap -> Coord -> (Mine,Int) -> MineFlash
absorb mp c (mine,flsh) = (mabsorb mine neigh,flsh) where
    neigh = map fst . neighbours nrim c $ mp

-- collapse flashed states and increase amount of flashes
collapse :: MineMap -> Coord -> (Mine,Int) -> ValFlash
collapse mp c (v,flsh) = mcollapse v flsh

-- makes explosions into an exploding value (instead of collapse that sets to 0)
mine :: MineMap -> Coord -> (Mine,Int) -> Int
mine mp c (Expl,_)     = 10
mine mp c (Inac v n,_) = v

value :: FlashMap -> Coord -> (Int,Int) -> Int
value mp c (v,_) = v

flash :: FlashMap -> Coord -> (Int,Int) -> Int
flash mp c (_,f) = f

starter :: Map -> FlashMap
starter = gridapply init'

explgrid :: MineMap -> Map
explgrid = gridapply mine

valuegrid :: FlashMap -> Map
valuegrid = gridapply value

flashgrid :: FlashMap -> Map
flashgrid = gridapply flash

flashstep :: MineMap -> MineMap
flashstep = gridapply absorb

multiflash :: MineMap -> MineMap
multiflash mp = if stp /= mp
                then multiflash stp
                else mp where
                stp = flashstep mp

step :: FlashMap -> FlashMap
step = gridapply collapse . multiflash . gridapply energize

multistep :: Int -> FlashMap -> FlashMap
multistep 0 mp = mp
multistep n mp = multistep (n-1) (step mp)

flashes :: FlashMap -> Int
flashes = sum . map sum . flashgrid

-- solution
solve :: [String] -> Int
solve = flashes . multistep 100 . starter . parse

solve' :: [String] -> Int
solve' xs = 2

main :: IO ()
main = do
    file <- readFile "input/day11.txt"
    let l = lines file
    print $ solve l
    print $ solve' l

test :: IO Map
test = do
    file <- readFile "input/test11.txt"
    let l = lines file
    return (parse l)
