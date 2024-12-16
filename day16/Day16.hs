import Data.Array
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Grid = Array Position Char

type Position = (Int, Int)

type Score = Int

data Direction = North | South | West | East deriving (Show)

parse :: String -> Grid
parse contents = listArray ((0, 0), (maxY, maxX)) $ concat gridLines
  where
    gridLines = lines contents
    maxY = length gridLines - 1
    maxX = length (gridLines !! 0) - 1

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft West = South
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight West = North
turnRight East = South

delta :: Direction -> Position
delta North = (-1, 0)
delta South = (1, 0)
delta West = (0, -1)
delta East = (0, 1)

nextPos :: Position -> Direction -> Position
nextPos (y, x) direction = let (dy, dx) = delta direction in (y + dy, x + dx)

search :: Grid -> Position -> Direction -> Set Position -> Maybe Score
search grid pos direction visited
  | cell == '#' = Nothing
  | cell == 'E' = Just 0
  | pos `elem` visited = Nothing
  | otherwise = score
  where
    cell = grid ! pos
    visited' = Set.insert pos visited
    scoreF = (+ 1) <$> search grid (nextPos pos direction) direction visited'
    posLeft = nextPos pos (turnLeft direction)
    scoreL = (+ 1001) <$> search grid posLeft (turnLeft direction) visited'
    posRight = nextPos pos (turnRight direction)
    scoreR = (+ 1001) <$> search grid posRight (turnRight direction) visited'
    score = case catMaybes [scoreF, scoreL, scoreR] of
      [] -> Nothing
      scores -> Just $ minimum scores

part1 :: Grid -> Score
part1 grid = fromJust $ search grid start East Set.empty
  where
    (maxY, _) = snd $ bounds grid
    start = (maxY - 1, 1)

main = do
  contents <- readFile "example1"
  let grid = parse contents
  print $ part1 grid
