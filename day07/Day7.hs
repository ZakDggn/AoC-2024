type Equation = (Int, [Int])

parse :: String -> [Equation]
parse = map parseLine . map words . lines
  where
    parseLine allNums =
      let target = read . init $ head allNums
          nums = map read $ tail allNums
       in (target, nums)

part1 :: [Equation] -> Int
part1 = sum . map fst . filter isPossible
  where
    isPossible (target, nums) = target `elem` getAnswers nums
    getAnswers [] = []
    getAnswers [x] = [x]
    getAnswers (x : y : xs) = getAnswers ((x + y) : xs) ++ getAnswers ((x * y) : xs)

part2 :: [Equation] -> Int
part2 = sum . map fst . filter isPossible
  where
    isPossible (target, nums) = target `elem` getAnswers nums
    getAnswers [] = []
    getAnswers [x] = [x]
    getAnswers (x : y : xs) =
      getAnswers ((x + y) : xs)
        ++ getAnswers ((x * y) : xs)
        ++ getAnswers ((combine x y) : xs)
    combine x y = read $ show x ++ show y

main = do
  equations <- parse <$> readFile "input"
  print $ part1 equations
  print $ part2 equations
