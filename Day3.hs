
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = (map head xs) : transpose (map tail xs)

bin2dec :: [Int] -> Int
bin2dec xs = sum vs where
    vs = map (\(a,b) -> a*b) vsvs
    vsvs = zip [2^n | n <- [0..]] rev
    rev = reverse xs

strtoia :: String -> [Int]
strtoia = map (read . pure :: Char -> Int)

keep :: [Int] -> [a] -> [a]
keep [] [] = []
keep (x:xs) (y:ys) = if x == 1
                     then y : keep xs ys
                     else keep xs ys

oxcond :: Int -> Int -> Bool
oxcond le su  = le-su > su

cocond le su = not (oxcond le su)

filt :: (Int -> Int -> Bool) -> Int -> [[Int]] -> [[Int]]
filt cond n xss = if cond le su
                  then keep invr xss
                  else keep row xss
                  where
    o = xss
    (le,su) = (length row, sum row)
    invr = map (1-) row
    row = head . (drop n) $ transpose xss

ffold :: (Int -> Int -> Bool) -> Int -> [[Int]] -> [Int]
ffold cond n xss = if length f == 1
                   then head f
                   else ffold cond (n+1) f
                   where
                   f = filt cond n xss 

solve :: [String] -> Int
solve xs = (bin2dec eps) * (bin2dec gam) where
    eps = map (1-) gam
    gam = map gfunc sip
    sip = zip ssum slen
    slen = map length trans
    ssum = map sum trans
    trans = transpose binxs
    binxs = map strtoia xs
    gfunc = \(a,b) -> if a > b-a then 1 else 0


solve' :: [String] -> Int
solve' xs = (bin2dec oxbins) * (bin2dec cobins) where
    oxbins = ffold oxcond 0 bins
    cobins = ffold cocond 0 bins
    bins = map strtoia xs

main :: IO ()
main = do
    file <- readFile "input/day3.txt"
    let l = lines file
    print $ solve l
    print $ solve' l
