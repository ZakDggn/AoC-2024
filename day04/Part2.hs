part2 :: [String] -> Int
part2 grid = sum [1 | x <- [1 .. maxX], y <- [1 .. maxY], isXMAS x y]
  where
    maxY = length grid - 2
    maxX = length (grid !! 0) - 2
    isXMAS x y =
      let nw = getElem (x - 1) (y - 1)
          ne = getElem (x + 1) (y - 1)
          sw = getElem (x - 1) (y + 1)
          se = getElem (x + 1) (y + 1)
       in getElem x y == 'A'
            && [nw, se] `elem` ["MS", "SM"]
            && [ne, sw] `elem` ["MS", "SM"]
    getElem x y = grid !! y !! x

main = do
  grid <- lines <$> readFile "input"
  print $ part2 grid
