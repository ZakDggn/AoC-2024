import Data.Array
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Position = (Int, Int)

type Grid = Array Position Char

parse :: String -> Int -> Int -> (Grid, [Position])
parse contents n maxCoord = (grid, notFallen)
  where
    coordinates = map (\[x, y] -> (read x, read y)) . map (splitOn ",") $ lines contents
    (fallen, notFallen) = splitAt n coordinates
    grid = array ((0, 0), (maxCoord, maxCoord)) $ do
      x <- [0 .. maxCoord]
      y <- [0 .. maxCoord]
      let char = if (x, y) `elem` fallen then '#' else '.'
      return ((x, y), char)

getAdjacents :: Grid -> Position -> Set Position
getAdjacents grid (x, y) = Set.filter isValid adjacents
  where
    adjacents = Set.fromList [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
    isSpace pos = grid ! pos == '.'
    isValid pos = inRange (bounds grid) pos && isSpace pos

search :: Grid -> Maybe Int
search grid = go (Set.singleton start) Set.empty 0
  where
    start = (0, 0)
    end = snd $ bounds grid
    go positions visited steps
      | null positions = Nothing
      | end `elem` positions = Just steps
      | otherwise = go positions' visited' (steps + 1)
      where
        nextPositions = Set.foldr (addAdjacents) Set.empty positions
        addAdjacents pos = Set.union (getAdjacents grid pos)
        positions' = Set.difference nextPositions visited
        visited' = Set.union visited positions

part1 :: Grid -> Int
part1 grid = fromJust $ search grid

part2 :: Grid -> [Position] -> Position
part2 grid (pos : notFallen) = case search grid' of
  Just _steps -> part2 grid' notFallen
  Nothing -> pos
  where
    grid' = grid // [(pos, '#')]

main = do
  contents <- readFile "input"
  let (grid, notFallen) = parse contents 1024 70
  print $ part1 grid
  print $ part2 grid notFallen
