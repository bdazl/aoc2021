dir :: String -> (Int, Int)
dir "forward" = (1, 0)
dir "up" = (0, -1)
dir "down" = (0, 1)

op2 :: (Int,Int,Int) -> String -> Int -> (Int,Int,Int)
op2 (hor,vert,aim) "forward" val = (hor+val, vert+(aim*val), aim)
op2 (hor,vert,aim) "up" val = (hor, vert, aim-val)
op2 (hor,vert,aim) "down" val = (hor, vert, aim+val)

cmdval :: String -> (String,Int)
cmdval s = (a, bi) where
    bi = read b :: Int
    (a, b) = (head ab,last ab)
    ab = words s

apply2 :: (Int,Int,Int) -> String -> (Int,Int,Int)
apply2 state s = op2 state cmd val where
    (cmd, val) = cmdval s

psum :: (Int,Int) -> (Int,Int) -> (Int,Int)
psum a b = (fst a + fst b, snd a + snd b)

parse :: String -> (Int, Int)
parse s = (e1*bi, e2*bi) where
    (e1, e2) = dir a
    (a, bi) = cmdval s

solve :: [String] -> Int
solve xs = fst sum * snd sum where
    sum = foldl psum (0,0) (map parse xs)

solve' :: [String] -> Int
solve' xs = a*b where
    (a,b,c) = foldl apply2 (0,0,0) xs

main :: IO ()
main = do
    file <- readFile "input/day2.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
