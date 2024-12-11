parse :: String -> [Int]
parse = map read . words

transformStone :: Int -> [Int]
transformStone 0 = [1]
transformStone stone
  | evenDigits = [read left, read right]
  | otherwise = [stone * 2024]
  where
    digits = show stone
    len = length digits
    evenDigits = len `mod` 2 == 0
    (left, right) = splitAt (len `div` 2) digits

countStones :: Int -> Int -> Int
countStones 1 stone = length $ transformStone stone
countStones n stone = sum $ map (countStones (n - 1)) (transformStone stone)

part1 :: [Int] -> Int
part1 = sum . map (countStones 25)

main = do
  contents <- readFile "input"
  let stones = parse contents
  print $ part1 stones
