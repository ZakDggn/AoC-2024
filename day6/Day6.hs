import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Show, Enum)

type Position = (Int, Int)

type Obstacles = Map Position [Direction]

type Dimensions = (Int, Int)

parse :: String -> (Obstacles, Position, Dimensions)
parse contents = (obstacles, pos, (rows, cols))
  where
    grid = lines contents
    rows = length grid
    cols = length (grid !! 0)
    maxY = rows - 1
    maxX = cols - 1
    pos = head [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], grid !! y !! x == '^']
    obstacles = Map.fromList [((x, y), []) | x <- [0 .. maxX], y <- [0 .. maxY], grid !! y !! x == '#']

rotate :: Direction -> Direction
rotate West = North
rotate direction = succ direction

step :: Obstacles -> Position -> Direction -> (Position, Direction)
step obstacles pos direction
  | isObstacle pos' = (pos, rotate direction)
  | otherwise = (pos', direction)
  where
    (posX, posY) = pos
    pos' = (posX + dx, posY + dy)
    (dx, dy) = case direction of
      North -> (0, -1)
      East -> (1, 0)
      South -> (0, 1)
      West -> (-1, 0)
    isObstacle (x, y) = Map.member (x, y) obstacles

getVisited :: Obstacles -> Dimensions -> (Position, Direction) -> Set Position
getVisited obstacles (rows, cols) (pos, direction)
  | isOutOfBounds pos' = Set.singleton pos
  | otherwise = Set.insert pos $ getVisited obstacles (rows, cols) (pos', direction')
  where
    (pos', direction') = step obstacles pos direction
    isOutOfBounds (x, y) = x < 0 || x >= cols || y < 0 || y >= rows

part1 :: Obstacles -> Dimensions -> Position -> Int
part1 obstacles dimensions startPos = Set.size $ getVisited obstacles dimensions (startPos, North)

main = do
  contents <- readFile "input"
  let (obstacles, pos, dimensions) = parse contents
  print $ part1 obstacles pos dimensions
