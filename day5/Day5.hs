import Data.List (inits, partition, tails)
import Data.List.Split (splitOn)

type Page = Int

type Rules = [(Page, Page)]

type Update = [Page]

parse :: String -> (Rules, [Update])
parse contents = (rules, updates)
  where
    (ruleStrings, updateStrings) = tuplify . map lines . splitOn "\n\n" $ contents
    rules = map (tuplify . map read . splitOn "|") ruleStrings
    updates = map (map read . splitOn ",") updateStrings
    tuplify [x, y] = (x, y)

okPair :: Rules -> Page -> Page -> Bool
okPair rules p1 p2 = not $ (p2, p1) `elem` rules

checkPartial :: Rules -> Update -> Bool
checkPartial rules update =
  length update <= 1 || check update
  where
    check (p : ps) = and $ map (okPair rules p) ps

isCorrect :: Rules -> Update -> Bool
isCorrect rules update = and . map (checkPartial rules) $ tails update

getMiddle :: Update -> Page
getMiddle update = update !! ((length update) `div` 2)

part1 :: [Update] -> Int
part1 = sum . map getMiddle

sort :: Rules -> Update -> Update
sort rules = foldr insert []
  where
    insert page [] = [page]
    insert page (p : ps)
      | okPair rules page p = page : p : ps
      | otherwise = p : (insert page ps)

part2 :: Rules -> [Update] -> Int
part2 rules = sum . map getMiddle . map (sort rules)

main = do
  contents <- readFile "input"
  let (rules, updates) = parse contents
  let (correctUpdates, incorrectUpdates) = partition (isCorrect rules) updates
  print $ part1 correctUpdates
  print $ part2 rules incorrectUpdates
