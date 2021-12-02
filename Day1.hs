import Common

window :: [Int] -> [(Int,Int,Int)]
window (a:b:c:xs) = (a,b,c):(window (b:c:xs))
window _ = []

solve :: [Int] -> Int
solve xs = o where
    o = count (==True) b
    b = tail $ map pcomp z
    z = zip xs (1:xs)
    pcomp = \(a,b) -> a>b

solve' :: [Int] -> Int
solve' xs = solve s where
    s = map (\(a, b, c) -> a+b+c) w
    w = window xs

main :: IO ()
main = do
    file <- readFile "input/day1.txt"
    let l = lines file
    let i = map atoi l
    print $ solve i
    print $ solve' i
