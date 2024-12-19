import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import Data.MemoTrie

parse :: String -> ([String], [String])
parse contents = (patterns, designs)
  where
    patterns = words . filter (/= ',') . head $ lines contents
    designs = drop 2 $ lines contents

isPossible :: [String] -> String -> Bool
isPossible patterns = memoFix helper
  where
    helper _ "" = True
    helper f design
      | null designSuffixes = False
      | otherwise = any f designSuffixes
      where
        designSuffixes = catMaybes $ map (`stripPrefix` design) patterns

waysPossible :: [String] -> String -> Int
waysPossible patterns = memoFix helper
  where
    helper _ "" = 1
    helper f design
      | null designSuffixes = 0
      | otherwise = sum $ map f designSuffixes
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
