import Data.List (isPrefixOf, transpose)

countOccurences :: String -> Int
countOccurences "" = 0
countOccurences str
  | "XMAS" `isPrefixOf` str = succ $ countOccurences $ drop 4 str
  | otherwise = countOccurences $ drop 1 str

getDiagonals :: [String] -> [String]
getDiagonals grid = rightDiagonals ++ leftDiagonals
  where
    width = length (grid !! 0)
    rightDiagonals = [getDiagonal (+) i | i <- [-width + 1 .. width - 1]]
    leftDiagonals = [getDiagonal (-) i | i <- [0 .. 2 * (width - 1)]]
    getDiagonal f i =
      [ grid !! y !! x
        | y <- [0 .. width - 1],
          let x = f i y,
          0 <= x,
          x < width
      ]

part1 :: [String] -> Int
part1 = sum . map countOccurences . addReversed . searchLines
  where
    searchLines = concat . sequence [id, transpose, getDiagonals]
    addReversed = concatMap (\line -> [line, reverse line])

main = do
  grid <- lines <$> readFile "input"
  print $ part1 grid
