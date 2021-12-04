module Day4 where
import Common

type Marker = Maybe Int
type Board = [[Marker]]

anytrue = any (==True)

val :: Marker -> Int
val Nothing  = 0
val (Just n) = n

parseres :: [String] -> ([Int], [String])
parseres xs = (res, tail restraw) where
    res = map atoi ress
    ress = wordsWhen (==',') resin
    (resin, restraw) = (head xs, tail xs)

parseb :: Board -> String -> (Board, Bool)
parseb b "\n" = (b, True)
parseb b "" = (b, True)
parseb b xs = (b ++ [newj], False) where
    newj = map Just new
    new = map atoi (words xs)

parseboards :: [String] -> Board -> [Board] -> [Board]
parseboards [] b ys = b:ys
parseboards (x:xs) b ys = if new
                          then parseboards xs [] (upb:ys)
                          else parseboards xs upb ys
                          where
                          (upb, new) = parseb b x

parse :: [String] -> ([Int], [Board])
parse xs = (res, boards) where
    boards = parseboards rest [] []
    (res, rest) = parseres xs

mswap :: Int -> Marker -> Marker
mswap n Nothing = Nothing
mswap n (Just x) = if n == x
                   then Nothing
                   else Just x

mark :: Int -> Board -> Board
mark n b = map (map (mswap n)) b

markall :: Int -> [Board] -> [Board]
markall n bs = map (mark n) bs

won' :: [Marker] -> Bool
won' = all (==Nothing)

won :: Board -> Bool
won b = anytrue colw || anytrue roww where
    colw = map won' (transpose b)
    roww = map won' b

bsum' :: [Marker] -> Int
bsum' ms = sum $ map val ms

bsum :: Board -> Int
bsum = sum . map bsum'

exec :: [Int] -> [Board] -> (Int, Board)
exec (x:xs) bs = if anytrue ws
                 then (x, head (filter won new))
                 else exec xs new
                 where
    ws = map won new
    new = markall x bs

exec' :: [Int] -> [Board] -> (Int, Board)
exec' xs [b] = finalize xs b
exec' (x:xs) bs = exec' xs nowin 
                  where
    nowin = filter (not . won) new
    new = markall x bs

finalize :: [Int] -> Board -> (Int, Board)
finalize (x:xs) b = if w
                    then (x, new)
                    else finalize xs new
                    where
                    w = won new
                    new = mark x b

solve :: [String] -> Int
solve xs = s * v where
    s = bsum b
    (v, b) = exec res brds 
    (res, brds) = parse xs

solve' :: [String] -> Int
solve' xs = s * v where
    s = bsum b
    (v, b) = exec' res brds 
    (res, brds) = parse xs

main :: IO ()
main = do
    file <- readFile "input/day4.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
