import Data.List (inits, partition, tails)
import Data.List.Split (splitOn)

type Rules = [(Int, Int)]

type Update = [Int]

data Direction = Forwards | Backwards

tuplify :: (Show a) => [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify list = error $ "Couldn't tuplify list: " ++ (show list)

parse :: String -> (Rules, [Update])
parse contents = (rules, updates)
  where
    (ruleStrings, updateStrings) = tuplify . map lines . splitOn "\n\n" $ contents
    rules = map (tuplify . map read . splitOn "|") ruleStrings
    updates = map (map read . splitOn ",") updateStrings

okPair :: Rules -> Int -> Int -> Bool
okPair rules p1 p2 = not $ (p1, p2) `elem` rules

checkPartial :: Direction -> Rules -> Update -> Bool
checkPartial direction rules update =
  length update <= 1 || case direction of
    Forwards -> check update flip
    Backwards -> check (reverse update) id
  where
    check (p : ps) order = and $ map (order (okPair rules) p) ps

isCorrect :: Rules -> Update -> Bool
isCorrect rules update = isCorrectForwards && isCorrectBackwards
  where
    isCorrectForwards = and . map (checkPartial Forwards rules) $ tails update
    isCorrectBackwards = and . map (checkPartial Backwards rules) $ inits update

getMiddle :: Update -> Int
getMiddle update = update !! ((length update) `div` 2)

part1 :: [Update] -> Int
part1 = sum . map getMiddle

correctOrder :: Rules -> Update -> Update
correctOrder rules = foldr insert []
  where
    insert page [] = [page]
    insert page (p : ps)
      | okPair rules page p = p : (insert page ps)
      | otherwise = page : p : ps

part2 :: Rules -> [Update] -> Int
part2 rules = sum . map getMiddle . map (correctOrder rules)

main = do
  contents <- readFile "input"
  let (rules, updates) = parse contents
  let (correctUpdates, incorrectUpdates) = partition (isCorrect rules) updates
  print $ part1 correctUpdates
  print $ part2 rules incorrectUpdates
