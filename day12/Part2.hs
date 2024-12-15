import Data.Array
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set

type Plot = (Int, Int)

type Region = Set Plot

type Grid = Array Plot Char

type Corner = (Float, Float)

parse :: String -> Grid
parse contents = listArray ((0, 0), (maxY, maxX)) (concat rows)
  where
    rows = lines contents
    maxY = length rows - 1
    maxX = length (rows !! 0) - 1

exploreRegion :: Grid -> Plot -> Region -> Region
exploreRegion grid plot@(y, x) visited
  | plot `elem` visited = visited
  | otherwise = foldr (exploreRegion grid) visited' nextPlots
  where
    plant = grid ! plot
    inRegion plot' = inRange (bounds grid) plot' && grid ! plot' == plant
    adjacents = [(y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)]
    nextPlots = filter (\plot' -> inRegion plot' && plot' `notElem` visited) adjacents
    visited' = Set.insert plot visited

findRegions :: Grid -> [Region]
findRegions grid = filter (not . null) allResults
  where
    allResults = foldr (\plot results -> findNextRegion plot results : results) [] plots
    (maxX, maxY) = snd $ bounds grid
    plots = [(y, x) | x <- [0 .. maxX], y <- [0 .. maxY]]
    findNextRegion plot results
      | any (plot `elem`) results = Set.empty
      | otherwise = exploreRegion grid plot Set.empty

plotCorners :: Plot -> [Corner]
plotCorners (yInt, xInt) = [(y - 0.5, x - 0.5), (y - 0.5, x + 0.5), (y + 0.5, x - 0.5), (y + 0.5, x + 0.5)]
  where
    y = fromIntegral yInt
    x = fromIntegral xInt

missedCorner :: Region -> Corner -> Bool
missedCorner region (y, x) =
  ((nw && se) && (not ne && not sw)) || ((ne && sw) && (not nw && not se))
  where
    nw = (round (y - 0.5), round (x - 0.5)) `elem` region
    ne = (round (y - 0.5), round (x + 0.5)) `elem` region
    se = (round (y + 0.5), round (x + 0.5)) `elem` region
    sw = (round (y + 0.5), round (x - 0.5)) `elem` region

regionSides :: Region -> Int
regionSides region = missed + MS.distinctSize (MS.filter oddOccur corners)
  where
    corners = foldr MS.insert MS.empty . concatMap plotCorners $ Set.toList region
    missed = MS.fold (\corner count -> if missedCorner region corner then count + 1 else count) 0 corners
    oddOccur corner = MS.occur corner corners `mod` 2 /= 0

part2 :: Grid -> Int
part2 grid = sum $ zipWith (*) areas sides
  where
    regions = findRegions grid
    areas = map Set.size regions
    sides = map regionSides regions

main = do
  contents <- readFile "input"
  let grid = parse contents
  print $ part2 grid
