import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Files = Map Int Int -- Map id length

type Spaces = [Int]

data Mode = File | Space

type State = (Mode, Set Int)

parse :: String -> (Files, Spaces)
parse contents = (files, spaces)
  where
    diskMap = map digitToInt . head . lines $ contents
    maxIndex = length diskMap - 1
    fileLengths = [diskMap !! i | i <- [0 .. maxIndex], i `mod` 2 == 0]
    files = Map.fromList $ zip [0 ..] fileLengths
    spaces = [diskMap !! i | i <- [0 .. maxIndex], i `mod` 2 /= 0]

compact :: State -> Files -> Spaces -> [Int]
compact (File, seen) files spaces = case Map.lookupMin files of
  Nothing -> []
  Just (iD, len) -> replicate len iD' ++ compact (Space, seen) files' spaces
    where
      iD' = if Set.member iD seen then 0 else iD
      files' = Map.delete iD files
compact (Space, _) _ [] = []
compact (Space, seen) files (0 : spaces) = compact (File, seen) files spaces
compact (Space, seen) files (spaceLen : spaces) = case fileToMove of
  Nothing -> replicate spaceLen 0 ++ compact (File, seen') files spaces
  Just (iD, fileLen) ->
    let seen'' = Set.insert iD seen'
        spaces' = (spaceLen - fileLen) : spaces
     in replicate fileLen iD ++ compact (Space, seen'') files spaces'
  where
    fileToMove = Map.lookupMax $ Map.filter (<= spaceLen) $ Map.withoutKeys files seen
    (maxID, _) = Map.findMax files
    seen' = Set.insert maxID seen

part2 :: Files -> Spaces -> Int
part2 files spaces = checksum $ compact (File, Set.empty) files spaces
  where
    checksum = sum . zipWith (*) [0 ..]

main = do
  contents <- readFile "input"
  let (files, spaces) = parse contents
  print $ part2 files spaces
