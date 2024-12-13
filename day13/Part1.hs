import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

data Machine = Machine (Int, Int) (Int, Int) (Int, Int)

parse :: String -> [Machine]
parse = map (makeMachine . map parseNums . lines) . splitOn "\n\n"
  where
    parseNums = map read . splitOn "," . filter (\c -> isDigit c || c == ',')
    makeMachine [[aX, aY], [bX, bY], [pX, pY]] = Machine (aX, aY) (bX, bY) (pX, pY)

getCombination :: Machine -> Int -> Maybe (Int, Int)
getCombination (Machine (aX, aY) (bX, bY) (pX, pY)) a = combination
  where
    xLeft = pX - a * aX
    yLeft = pY - a * aY
    combination = case (xLeft `divMod` bX, yLeft `divMod` bY) of
      ((b, 0), (b', 0)) -> if b == b' then Just (a, b) else Nothing
      _ -> Nothing

findCombinations :: Machine -> [(Int, Int)]
findCombinations = filter isValid . catMaybes . getCombinations
  where
    getCombinations machine = map (getCombination machine) [0 .. 100]
    isValid (_a, b) = b <= 100

part1 :: [Machine] -> Int
part1 = sum . minTokens . filter (not . null) . map findCombinations
  where
    minTokens = map (minimum . map cost)
    cost (a, b) = 3 * a + b

main = do
  contents <- readFile "input"
  let machines = parse contents
  print $ part1 machines
