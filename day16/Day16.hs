import Data.List (nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

type Score = Int

data Direction = North | East | South | West deriving (Enum, Eq, Ord)

type Maze = Map Position (Map Direction Score)

type CellState = (Position, Direction, Score)

parse :: String -> (Maze, Position, Position)
parse contents = (Map.fromList maze, (maxY - 1, 1), (1, maxX - 1))
  where
    grid = lines contents
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    maze = [((y, x), Map.empty) | y <- [0 .. maxY], x <- [0 .. maxX], grid !! y !! x /= '#']

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft direction = pred direction

turnRight :: Direction -> Direction
turnRight West = North
turnRight direction = succ direction

delta :: Direction -> Position
delta North = (-1, 0)
delta East = (0, 1)
delta South = (1, 0)
delta West = (0, -1)

nextPos :: Position -> Direction -> Position
nextPos (y, x) direction = let (dy, dx) = delta direction in (y + dy, x + dx)

nextCells :: CellState -> [CellState]
nextCells (pos, direction, score) = [forward, left, right]
  where
    forward = (nextPos pos direction, direction, score + 1)
    left = (pos, turnLeft direction, score + 1000)
    right = (pos, turnRight direction, score + 1000)

betterThanCurrent :: Maze -> CellState -> Bool
betterThanCurrent maze (pos, direction, score) = case Map.lookup pos maze of
  Nothing -> False
  Just scoreMap -> case Map.lookup direction scoreMap of
    Nothing -> True
    Just bestScore -> if score < bestScore then True else False

updateMaze :: CellState -> Maze -> Maze
updateMaze cell@(pos, direction, score) maze
  | betterThanCurrent maze cell = Map.adjust (Map.insert direction score) pos maze
  | otherwise = maze

step :: Maze -> [CellState] -> (Maze, [CellState])
step maze cells = (maze', adjacents')
  where
    maze' = foldr updateMaze maze cells
    adjacents = concatMap nextCells cells
    adjacents' = nub $ filter (betterThanCurrent maze) adjacents

search :: Maze -> [CellState] -> Position -> [Score] -> [Score]
search _ [] _ scores = scores
search maze cells end scores = search maze' cells' end scores'
  where
    (ends, unvisited) = partition ((== end) . getPosition) cells
    scores' = scores ++ (map getScore ends)
    (maze', cells') = step maze unvisited
    getPosition (pos, _, _) = pos
    getScore (_, _, score) = score

part1 :: Maze -> Position -> Position -> Score
part1 maze start end = minimum $ search maze [(start, East, 0)] end []

main = do
  contents <- readFile "input"
  let (maze, start, end) = parse contents
  print $ part1 maze start end
