module Common
( atoi
, btoi
, chtoi
, splitN
, modifyN
, count
, enumerate
, clamp
, wordsWhen
, transpose
, gen
, sign
, rng
) where

import System.Environment

enumerate :: [a] -> [(Int, a)]
enumerate = zip (map fromIntegral [0..])

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

btoi :: Bool -> Int
btoi True  = 1
btoi False = 0

atoi :: String -> Int
atoi s = read s :: Int

chtoi :: Char -> Int
chtoi = read . pure

splitN :: Int -> [a] -> ([a], [a])
splitN n l = (take n l, drop n l)

modifyN :: Int -> a -> [a] -> [a]
modifyN n new l = y where
    y = map (\(i,x) -> if i == n then new else x) enum
    enum = zip [0..] l

-- Given a predicate, count the number of occorences in a list
count :: (a -> Bool) -> [a] -> Int
count pred xs = length . (filter pred) $ xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- use import Data.List, it's there...
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = (map head xs) : transpose (map tail xs)

-- generate (apparently this is a re-implementation of the Prelude function: replicate)
-- use replicate instead
gen :: Int -> a -> [a]
gen 0 _ = []
gen n x = x:gen (n-1) x

sign :: Int -> Int
sign a | a < 0 = -1
       | a == 0 = 0
       | otherwise = 1

-- rng (range) is similar to [a..b], but also works when a > b
rng :: Int -> Int -> [Int]
rng a b | a == b = [a]
        | otherwise = a : rng (a+(sign (b-a))) b
