import Data.MemoTrie

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
countStones = curry $ memoFix helper
  where
    helper _ (0, _) = 1
    helper f (n, stone) = sum $ map (curry f (n - 1)) (transformStone stone)

part2 :: [Int] -> Int
part2 = sum . map (countStones 75)

main = do
  contents <- readFile "input"
  let stones = parse contents
  print $ part2 stones
