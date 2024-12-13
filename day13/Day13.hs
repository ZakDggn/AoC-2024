import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

data Machine = Machine (Integer, Integer) (Integer, Integer) (Integer, Integer)

parse :: String -> [Machine]
parse = map (makeMachine . map parseNums . lines) . splitOn "\n\n"
  where
    parseNums = map read . splitOn "," . filter (\c -> isDigit c || c == ',')
    makeMachine [[aX, aY], [bX, bY], [pX, pY]] = Machine (aX, aY) (bX, bY) (pX, pY)

solveMachine :: Machine -> Maybe (Integer, Integer)
solveMachine (Machine (aX, aY) (bX, bY) (pX, pY))
  | aIsIntegral && bIsIntegral = Just (a, b)
  | otherwise = Nothing
  where
    m = pX * bY - pY * bX
    n = aX * bY - aY * bX
    u = pX - a * aX
    v = bX
    a = m `div` n
    b = u `div` v
    aIsIntegral = m `mod` n == 0
    bIsIntegral = u `mod` v == 0

minTokens :: [Machine] -> Integer
minTokens = sum . map cost . catMaybes . map solveMachine
  where
    cost (a, b) = 3 * a + b

part1 :: [Machine] -> Integer
part1 = minTokens

part2 :: [Machine] -> Integer
part2 = minTokens . map correct
  where
    increase = 10000000000000
    correct (Machine a b (pX, pY)) = Machine a b (pX + increase, pY + increase)

main = do
  contents <- readFile "input"
  let machines = parse contents
  print $ part1 machines
  print $ part2 machines
