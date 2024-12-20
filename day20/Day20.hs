import Data.Array
import qualified Data.Map as Map

type Position = (Int, Int)

type Grid = Array Position Char

parse :: String -> (Grid, Position)
parse contents = (grid, start)
  where
    rows = lines contents
    maxY = length rows - 1
    maxX = length (rows !! 0) - 1
    grid = listArray ((0, 0), (maxY, maxX)) $ concat rows
    [start] = [(y, x) | y <- [0 .. maxY], x <- [0 .. maxX], rows !! y !! x == 'S']

adjacents :: Position -> [Position]
adjacents (y, x) = [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]

inBounds :: Grid -> Position -> Bool
inBounds grid = inRange $ bounds grid

findPath :: Grid -> Position -> [Position]
findPath grid start = reverse $ go start start
  where
    go curr prev
      | grid ! curr == 'E' = [curr]
      | otherwise = curr : (go next curr)
      where
        [next] = filter isValid $ adjacents curr
        isValid pos = inBounds grid pos && grid ! pos /= '#' && pos /= prev

findCheats :: Grid -> [Position] -> [Int]
findCheats grid path = go path 0
  where
    distanceMap = Map.fromList $ zip path [0 ..]
    go [] _ = []
    go (pathPos : rest) dist = cheats ++ go rest (dist + 1)
      where
        walls = filter isWall $ adjacents pathPos
        isWall pos = inBounds grid pos && grid ! pos == '#'
        skipPositions = filter isValid $ concatMap adjacents walls
        isValid pos = inBounds grid pos && pos /= pathPos && Map.member pos distanceMap
        cheats = map ((dist - 2 -) . ((Map.!) distanceMap)) skipPositions

part1 :: Grid -> Position -> Int
part1 grid start = length . filter (>= 100) . findCheats grid $ findPath grid start

main = do
  contents <- readFile "input"
  let (grid, start) = parse contents
  print $ part1 grid start
