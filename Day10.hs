module Day10 where
import Data.List

data Elem = Ignore | Bad Char | Open Char | Close Char
            deriving Show
type Stack = [Elem]

-- stack and element management
newelem :: Char -> Elem
newelem c | isopen c = Open c
          | isclose c = Close c
          | otherwise = Ignore

push :: Stack -> Elem -> Stack
push xs (Open e)  = (Open e):xs
push xs (Close e) = close xs e
push xs Ignore    = xs
push xs (Bad e)   = (Bad e):xs

closes :: Elem -> Elem -> Bool
closes (Open o) (Close c) = omatchc o c
closes _ _ = False

close :: Stack -> Char -> Stack
close [] e = [Bad e]
close (x:xs) c = if closes x (Close c)
                 then xs
                 else (Bad c):x:xs

isbad :: Elem -> Bool
isbad (Bad _) = True
isbad _ = False

-- the first bad occurrence is actually the last in the stack
firstbad :: Stack -> Maybe Elem
firstbad xs = if length badies > 0
              then Just (last badies) 
              else Nothing where
    badies = filter isbad xs

-- build stack from string
build :: Stack -> String -> Stack
build s [] = s
build s (x:xs) = build (push s (newelem x)) xs

-- calculate error score
score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

-- look at openers, instead of what to close with (saves a flip operation)
score' :: Char -> Int
score' '(' = 1
score' '[' = 2
score' '{' = 3
score' '<' = 4

escore :: Maybe Elem -> Int
escore (Just (Bad e)) = score e
escore _ = 0

-- first task is to get the error of a bracket line
lineerr :: String -> Int
lineerr xs = escore mbad where
    mbad = firstbad stck
    stck = build [] xs

-- second task is to get the score of correctly closing a bracket line
linescore :: String -> Int
linescore xs = if err > 0 then 0 else calcscore 0 (build [] xs) where
    err = lineerr xs

calcscore :: Int -> Stack -> Int
calcscore s [] = s
calcscore s ((Open e):xs) = calcscore (5*s + score' e) xs
calcscore s (x:xs) = calcscore s xs

-- helpers
obracks :: String
obracks = "([{<"

cbracks :: String
cbracks = ")]}>"

isin :: Eq a => [a] -> a -> Bool
isin [] _     = False
isin (x:xs) y = if x == y then True else isin xs y

isopen :: Char -> Bool
isopen = isin obracks

isclose :: Char -> Bool
isclose = isin cbracks

omatchc :: Char -> Char -> Bool
omatchc o c = sum flt == 1 where
    flt = [1 | (a,b) <- zip obracks cbracks, a == o, b == c]

-- solution
solve :: [String] -> Int
solve = sum . (map lineerr)

solve' :: [String] -> Int
solve' xs = scoreboard !! div l 2 where
    l = length scoreboard
    scoreboard = sort . filter (>0) . (map linescore) $ xs

main :: IO ()
main = do
    file <- readFile "input/day10.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
