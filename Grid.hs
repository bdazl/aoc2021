module Grid where
import Data.Maybe
import Common

-- im hoping that this will come in handy for other problems
type Grid a = [[a]]
type Coord = (Int, Int)

-- cut a list down to an index and its neighbour(s)
-- rowcut 2 [1, 2, 3, 4] = [2, 3, 4]
rowcut :: Int -> [a] -> [a]
rowcut n xs = take 3 . drop (n-1) $ xs

-- given a coord and a grid, try cutting the grid to a square
-- of neighbours surrounding the coord (including the input pt itself)
-- this works for edges as well, but then the dimensions differ.
-- the output is a sub-grid as well as a coord in the output grid that
-- corresponds to the input coord of the original map
nsquare :: Grid a -> Coord -> (Grid a, Coord)
nsquare mp (x,y) = (sqr, (nx,ny)) where
    sqr = map (rowcut x) . take (2+ny) . drop (y-1) $ mp
    ny = if y == 0 then 0 else 1
    nx = if x == 0 then 0 else 1

-- get value for coordinate of grid
get :: Grid a -> Coord -> Maybe a
get mp (x,y) = if insqr (x,y)
               then Just ((mp !! y) !! x)
               else Nothing where
    (dimx, dimy) = (length (head mp), length mp)
    insqr (a,b) = xinsqr a && yinsqr b
    xinsqr x_ = x_ >= 0 && x_ < dimx
    yinsqr y_ = y_ >= 0 && y_ < dimy

-- pick a pt on the grid and get its neighbours
-- the neighbouring pts are determined by the input coordinate generator
neighbours :: (Coord -> [Coord]) -> Coord -> Grid a -> [a]
neighbours cgen (x,y) mp = catMaybes . map (get sqr) . cgen $ (sx, sy) where
    (sqr, (sx,sy)) = nsquare mp (x,y)

-- neighbouring coordinate pickers
nvert :: Coord -> [Coord]
nvert (x,y) = [(x, y-1), (x, y+1)]

nhoriz :: Coord -> [Coord]
nhoriz (x,y) = [(x-1, y), (x+1, y)]

ndiaglr :: Coord ->[Coord]
ndiaglr (x,y) = [(x-1, y-1), (x+1, y+1)]

ndiagrl :: Coord ->[Coord]
ndiagrl (x,y) = [(x+1, y-1), (x-1, y+1)]

nplus = combiner [nvert, nhoriz]
ndiag = combiner [ndiaglr, ndiagrl]
nrim = combiner [nplus, ndiag]

-- combining coordinate pickers
combiner :: [a -> [b]] -> a -> [b]
combiner xs v = foldr (++) [] . map (apply v) $ xs where
    apply a f = f a

-- apply function to all values in grid (that could become a new type of grid)
gridapply :: (Grid a -> Coord -> a -> b) -> Grid a -> Grid b
gridapply f mp = map rowop $ enumerate mp where
    rowop (y,row) = map (op y) $ enumerate row
    op y (x,v) = f mp (x,y) v
