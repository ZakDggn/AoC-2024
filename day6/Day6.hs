import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Eq, Enum, Show)

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

getDelta :: Direction -> (Int, Int)
getDelta North = (0, -1)
getDelta East = (1, 0)
getDelta South = (0, 1)
getDelta West = (-1, 0)

nextPos :: Position -> Direction -> Position
nextPos (x, y) direction = (x + dx, y + dy)
  where
    (dx, dy) = getDelta direction

step :: Obstacles -> Position -> Direction -> (Position, Direction)
step obstacles pos direction
  | isObstacle pos' = (pos, rotate direction)
  | otherwise = (pos', direction)
  where
    pos' = nextPos pos direction
    isObstacle (x, y) = Map.member (x, y) obstacles

isOutOfBounds :: Dimensions -> Position -> Bool
isOutOfBounds (rows, cols) (x, y) = x < 0 || x >= cols || y < 0 || y >= rows

getVisited :: Obstacles -> Dimensions -> (Position, Direction) -> Set Position
getVisited obstacles dimensions (pos, direction)
  | isOutOfBounds dimensions pos' = Set.singleton pos
  | otherwise = Set.insert pos visited
  where
    visited = getVisited obstacles dimensions (pos', direction')
    (pos', direction') = step obstacles pos direction

part1 :: Obstacles -> Dimensions -> Position -> Int
part1 obstacles dimensions startPos = Set.size $ getVisited obstacles dimensions (startPos, North)

wouldLoop :: Obstacles -> Dimensions -> Position -> Direction -> Bool
wouldLoop obstacles dimensions pos searchDirection
  | isOutOfBounds dimensions pos = False
  | foundObstacle = isLoopingObstacle
  | otherwise = wouldLoop obstacles dimensions pos' searchDirection
  where
    (pos', direction') = step obstacles pos searchDirection
    foundObstacle = direction' /= searchDirection
    obstaclePos = nextPos pos searchDirection
    -- TODO: might be a looping obstacle even if not visited yet?
    isLoopingObstacle = searchDirection `elem` (obstacles ! obstaclePos)

getLoops :: Obstacles -> Dimensions -> (Position, Direction) -> [Position]
getLoops obstacles dimensions (pos, direction)
  | isOutOfBounds dimensions pos' = []
  | otherwise = loopPositions'
  where
    (pos', direction') = step obstacles pos direction
    loopPositions = getLoops obstacles' dimensions (pos', direction')
    obstacles' =
      if direction /= direction'
        then Map.adjust (direction :) (nextPos pos direction) obstacles
        else obstacles
    searchDirection = rotate direction
    loopPositions' =
      if wouldLoop obstacles' dimensions pos searchDirection
        then (nextPos pos direction) : loopPositions
        else loopPositions

part2 :: Obstacles -> Dimensions -> Position -> Int
part2 obstacles dimensions startPos = length $ getLoops obstacles dimensions (startPos, North)

main = do
  contents <- readFile "input"
  let (obstacles, pos, dimensions) = parse contents
  print $ part1 obstacles dimensions pos
  print $ part2 obstacles dimensions pos
