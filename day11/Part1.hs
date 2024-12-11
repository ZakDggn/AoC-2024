parse :: String -> [Int]
parse = map read . words

transformOne :: Int -> [Int]
transformOne 0 = [1]
transformOne stone
  | evenDigits = [read left, read right]
  | otherwise = [stone * 2024]
  where
    digits = show stone
    len = length digits
    evenDigits = len `mod` 2 == 0
    (left, right) = splitAt (len `div` 2) digits

transformAll :: [Int] -> [Int]
transformAll = concatMap transformOne

part1 :: [Int] -> Int
part1 stones = length $ iterate transformAll stones !! 25

main = do
  contents <- readFile "input"
  let stones = parse contents
  print $ part1 stones
