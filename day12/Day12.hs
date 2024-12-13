import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

type Plot = (Int, Int)

type Region = Set Plot

type Grid = Array Plot Char

parse :: String -> Grid
parse contents = listArray ((0, 0), (maxY, maxX)) (concat rows)
  where
    rows = lines contents
    maxY = length rows - 1
    maxX = length (rows !! 0) - 1

exploreRegion :: Grid -> Plot -> (Region, Int) -> (Region, Int)
exploreRegion grid plot@(y, x) (visited, perimeter)
  | plot `elem` visited = (visited, perimeter)
  | otherwise = foldr (exploreRegion grid) (visited', perimeter') nextPlots
  where
    plant = grid ! plot
    inRegion plot' = inRange (bounds grid) plot' && grid ! plot' == plant
    adjacents = [(y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)]
    nextPlots = filter (\plot' -> inRegion plot' && plot' `notElem` visited) adjacents
    visited' = Set.insert plot visited
    perimeter' = perimeter + 4 - (length $ filter inRegion adjacents)

findRegions :: Grid -> [(Region, Int)]
findRegions grid = filter (not . null . fst) allResults
  where
    allResults = foldr (\plot results -> findNextRegion plot results : results) [] plots
    (maxX, maxY) = snd $ bounds grid
    plots = [(y, x) | x <- [0 .. maxX], y <- [0 .. maxY]]
    findNextRegion plot results
      | any (plot `elem`) (map fst results) = (Set.empty, 0)
      | otherwise = exploreRegion grid plot (Set.empty, 0)

part1 :: Grid -> Int
part1 = sum . map (\(region, perimeter) -> (Set.size region) * perimeter) . findRegions

main = do
  contents <- readFile "input"
  let grid = parse contents
  print $ part1 grid
