import Data.List (sortBy, stripPrefix)
import Data.Maybe (catMaybes)
import Data.MemoTrie

parse :: String -> ([String], [String])
parse contents = (patterns, designs)
  where
    patterns = words . filter (/= ',') . head $ lines contents
    designs = drop 2 $ lines contents

isPossible :: [String] -> String -> Bool
isPossible = curry $ memoFix helper
  where
    helper _ (_, "") = True
    helper f (patterns, design)
      | null designSuffixes = False
      | otherwise = any (curry f patterns) designSuffixes
      where
        designSuffixes = catMaybes $ map (`stripPrefix` design) patterns

waysPossible :: [String] -> String -> Int
waysPossible = curry $ memoFix helper
  where
    helper _ (_, "") = 1
    helper f (patterns, design)
      | null designSuffixes = 0
      | otherwise = sum $ map (curry f patterns) designSuffixes
      where
        designSuffixes = catMaybes $ map (`stripPrefix` design) patterns

part1 :: [String] -> [String] -> Int
part1 patterns designs = length $ filter (isPossible patterns) designs

part2 :: [String] -> [String] -> Int
part2 patterns designs = sum $ map (waysPossible patterns) designs

main = do
  contents <- readFile "input"
  let (patterns, designs) = parse contents
  print $ part1 patterns designs
  print $ part2 patterns designs
