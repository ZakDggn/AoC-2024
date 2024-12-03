parse :: String -> [[Int]]
parse = map (map read) . map words . lines

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

isSafe :: [Int] -> Bool
isSafe report = isSafeAsc report || isSafeAsc (reverse report)
  where
    isSafeAsc report = and $ zipWith isAscLevels (init report) (tail report)
    isAscLevels a b = (1 <= b - a) && (b - a <= 3)

part2 :: [[Int]] -> Int
part2 = length . filter isSafeDampened

isSafeDampened :: [Int] -> Bool
isSafeDampened report = isSafe report || containsSafeDampened
  where
    containsSafeDampened = any isSafe subReports
    subReports = [deleteAt i report | i <- [0 .. length report - 1]]
    deleteAt i xs = take i xs ++ drop (i + 1) xs

main = do
  contents <- readFile "input"
  let parsed = parse contents
  print $ part1 parsed
  print $ part2 parsed
