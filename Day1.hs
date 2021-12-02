import Common

window :: [Int] -> [(Int,Int,Int)]
window (a:b:c:xs) = (a,b,c):(window (b:c:xs))
window _ = []

solve2 :: [Int] -> Int
solve2 xs = solve1 s where
    s = map (\(a, b, c) -> a+b+c) w
    w = window xs
    

solve1 :: [Int] -> Int
solve1 xs = o where
    o = count (==True) b
    b = tail $ map pcomp z
    z = zip xs (1:xs)
    pcomp = \(a,b) -> a>b


test1 :: Int
test1 = solve1 testinp

testinp :: [Int]
testinp = [199,200,208,210,200,207,240,269,260,263]

main :: IO ()
main = do
    file <- readFile "input/day1.txt"
    let l = lines file
    let i = map atoi l
    print (solve2 i)
