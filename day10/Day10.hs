import Data.List (nub)

trailheads :: [[Char]] -> [(Int, Int)]
trailheads grid = [(x, y) | x <- [0..maxX], y <- [0..maxY], grid !! y !! x == '0']
  where
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1

findEnds :: [[Char]] -> (Int, Int) -> [(Int, Int)]
findEnds grid start@(x, y)
  | height == '9' = [start]
  | null nextPs = []
  | otherwise = concatMap (findEnds grid) nextPs
  where
    height = getCell start
    adjacents = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
    nextPs = filter (\p -> inBounds p && getCell p == succ height) adjacents
    getCell (i, j) = grid !! j !! i
    (rows, cols) = (length grid, length (grid !! 0))
    inBounds (i, j) = 0 <= i && i < cols && 0 <= j && j < rows

part1 :: [[Char]] -> Int
part1 grid = length . concat . map (nub . findEnds grid) $ trailheads grid

part2 :: [[Char]] -> Int
part2 grid = length . concatMap (findEnds grid) $ trailheads grid

main = do
  grid <- lines <$> readFile "input"
  print $ part1 grid
  print $ part2 grid
