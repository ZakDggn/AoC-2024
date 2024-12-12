import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as MS

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

transformAll :: IntMultiSet -> IntMultiSet
transformAll = MS.concatMap transformOne

solve :: Int -> [Int] -> Int
solve n = MS.size . (!! n) . iterate transformAll . MS.fromList

part1 :: [Int] -> Int
part1 = solve 25

part2 :: [Int] -> Int
part2 = solve 75

main = do
  contents <- readFile "input"
  let stones = parse contents
  print $ part1 stones
  print $ part2 stones
