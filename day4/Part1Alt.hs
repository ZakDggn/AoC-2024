getString :: (Int, Int) -> (Int, Int) -> [String] -> String
getString (x1, y1) (x2, y2) grid
  | x2 < 0 || x2 > maxX || y2 < 0 || y2 > maxY || (x1, y1) == (x2, y2) = ""
  | otherwise = [getElem x y | (x, y) <- zip xRange yRange]
  where
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    xRange = makeRange x1 x2
    yRange = makeRange y1 y2
    makeRange c1 c2
      | c1 == c2 = repeat c1
      | c1 > c2 = [c1, c1 - 1 .. c2]
      | otherwise = [c1 .. c2]
    getElem x y = grid !! y !! x

part1 :: [String] -> Int
part1 grid = sum [countXMAS x y | x <- [0 .. maxX], y <- [0 .. maxY], getElem x y == 'X']
  where
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    countXMAS x y = length $ filter (== "XMAS") $ allDirections x y
    deltas = [-3, 0, 3]
    allDirections x y = [getString (x, y) (x + dx, y + dy) grid | dx <- deltas, dy <- deltas]
    getElem x y = grid !! y !! x

main = do
  grid <- lines <$> readFile "input"
  print $ part1 grid
