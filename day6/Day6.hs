import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Show, Enum)

type Grid = [String]
type Position = (Int, Int)

parse :: String -> (Grid, Position)
parse contents = (grid, pos)
  where
    gridWithGuard = lines contents
    maxY = length gridWithGuard - 1
    maxX = length (gridWithGuard !! 0) - 1
    pos = head [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], gridWithGuard !! y !! x == '^']
    grid = map (map removeGuard) gridWithGuard
    removeGuard c = if c == '^' then '.' else c

rotate :: Direction -> Direction
rotate West = North
rotate direction = succ direction

getCell :: Grid -> (Int, Int) -> Char
getCell grid (x, y) = grid !! y !! x

step :: Grid -> Position -> Direction -> (Maybe Position, Direction)
step grid pos direction
  | isOutOfBounds pos' = (Nothing, direction)
  | isObstacle pos' = (Just pos, rotate direction)
  | otherwise = (Just pos', direction)
  where
    (posX, posY) = pos
    pos' = (posX + dx, posY + dy)
    (dx, dy) = case direction of
      North -> (0, -1)
      East -> (1, 0)
      South -> (0, 1)
      West -> (-1, 0)
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    isOutOfBounds (x, y) = x < 0 || x > maxX || y < 0 || y > maxY
    isObstacle (x, y) = getCell grid (x, y) == '#'

part1 :: Grid -> Position -> Int
part1 grid startPos = Set.size $ getVisited (Just startPos, North)
  where
    getVisited (pos, direction) = case pos of
      Nothing -> Set.empty
      Just pos' -> Set.insert pos' $ getVisited $ step grid pos' direction

main = do
  contents <- readFile "input"
  let (grid, pos) = parse contents
  print $ part1 grid pos
