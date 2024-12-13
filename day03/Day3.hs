import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

indexOf :: String -> String -> Maybe Int
indexOf "" _ = Nothing
indexOf _ "" = Nothing
indexOf needle haystack
  | needle `isPrefixOf` haystack = Just 0
  | otherwise = succ <$> indexOf needle (tail haystack)

indicesOf :: String -> String -> [Int]
indicesOf = indicesOf' 0

indicesOf' :: Int -> String -> String -> [Int]
indicesOf' offset needle haystack = case indexOf needle haystack of
  Nothing -> []
  Just index -> offset + index : indicesOf' nextOffset needle nextHaystack
    where
      nextOffset = offset + index + length needle
      nextHaystack = drop (index + length needle) haystack

indicesAfter :: String -> String -> [Int]
indicesAfter needle haystack = map (+ length needle) $ indicesOf needle haystack

getArgs :: String -> Int -> Maybe (Int, Int)
getArgs contents index
  | [arg1, arg2] <- splitOn "," args
  , (Just num1, Just num2) <- (readMaybe arg1, readMaybe arg2)
  = Just (num1, num2)
  | otherwise = Nothing
  where
    args = takeWhile (/= ')') $ drop index contents

getAllArgs :: String -> [Int] -> [(Int, Int)]
getAllArgs contents indices = catMaybes $ map (getArgs contents) indices

calcMuls :: String -> [Int] -> Int
calcMuls contents indices = sum $ map (\(a, b) -> a * b) $ getAllArgs contents indices

part1 :: String -> Int
part1 contents = calcMuls contents $ indicesAfter "mul(" contents

filterEnabled :: [Int] -> [Int] -> [Int] -> [Int]
filterEnabled muls dos donts = [mul | mul <- muls, isEnabled mul]
  where
    isEnabled mul = not $ any (mul `inRange`) disabledRanges
    inRange x (a, b) = a <= x && x <= b
    disabledRanges = [(dont, pair dont) | dont <- donts]
    pair dont = head $ dropWhile (< dont) dos

part2 :: String -> Int
part2 contents = calcMuls contents enabledMuls
  where
    muls = indicesAfter "mul(" contents
    dos = indicesAfter "do()" contents
    donts = indicesAfter "don't()" contents
    enabledMuls = filterEnabled muls dos donts

main = do
  contents <- readFile "input"
  print $ part1 contents
  print $ part2 contents
