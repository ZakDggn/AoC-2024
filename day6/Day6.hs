import Debug.Trace

data Direction = North | East | South | West deriving (Show, Enum)

type Position = (Int, Int)

parse :: String -> ([String], Position)
parse contents = (grid, pos)
  where
    gridWithGuard = lines contents
    maxY = length gridWithGuard - 1
    maxX = length (gridWithGuard !! 0) - 1
    pos = head [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], getCell gridWithGuard (x, y) == '^']
    grid = map (map removeGuard) gridWithGuard
    removeGuard c = if c == '^' then '.' else c

rotate :: Direction -> Direction
rotate West = North
rotate direction = succ direction

getCell :: [String] -> (Int, Int) -> Char
getCell grid (x, y) = grid !! y !! x

step :: [String] -> Position -> Direction -> ([String], Maybe Position, Direction)
step grid pos direction
  | isOutOfBounds pos' = (grid', Nothing, direction)
  | isObstacle = (grid, Just pos, rotate direction)
  | otherwise = (grid', Just pos', direction)
  where
    (x, y) = pos
    pos' = (x + dx, y + dy)
    (dx, dy) = case direction of
      North -> (0, -1)
      East -> (1, 0)
      South -> (0, 1)
      West -> (-1, 0)
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    grid' = [[char' | x <- [0 .. maxX], let char' = if (x, y) == pos then 'X' else getCell grid (x, y)] | y <- [0 .. maxY]]
    isObstacle = getCell grid pos' == '#'
    isOutOfBounds (x, y) = x < 0 || x > maxX || y < 0 || y > maxY

countVisits :: [String] -> Int
countVisits grid = length visited
  where
    maxY = length grid - 1
    maxX = length (grid !! 0) - 1
    visited = [() | x <- [0 .. maxX], y <- [0 .. maxY], getCell grid (x, y) == 'X']

part1 :: ([String], Maybe Position, Direction) -> Int
part1 (grid, Nothing, _) = countVisits grid
part1 (grid, (Just pos), direction) = trace (unlines grid ++ "\n" ++ show pos ++ ", " ++ show direction ++ "\n") part1 $ step grid pos direction

main = do
  contents <- readFile "input"
  let (grid, pos) = parse contents
  print $ part1 (grid, Just pos, North)
