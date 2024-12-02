import Data.List (transpose, sort)

fileToLists :: String -> ([Int], [Int])
fileToLists file = (intList !! 0, intList !! 1)
  where
    strList = transpose $ map words $ lines file
    intList = map sort $ map (map read) strList

totalDistance :: ([Int], [Int]) -> Int
totalDistance (left, right) = sum $ map abs $ zipWith (-) left right

similarity :: ([Int], [Int]) -> Int
similarity (left, right) = sum similarities
  where
    similarities = zipWith (*) left occurences
    occurences = [count n right | n <- left]
    count n ns = length $ filter (== n) ns

main = do
  contents <- readFile "input"
  let lists = fileToLists contents
  print $ totalDistance lists
  print $ similarity lists
