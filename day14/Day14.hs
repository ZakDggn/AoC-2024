import qualified Data.IntMultiSet as MS
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

data Robot = Robot (Int, Int) (Int, Int) deriving (Show)

type Bounds = (Int, Int)

parse :: String -> [Robot]
parse = map (makeRobot . map parseNums . words) . lines
  where
    parseNums = tuplify . map read . splitOn "," . drop 2
    tuplify [x, y] = (x, y)
    makeRobot [p, v] = Robot p v

update :: Bounds -> Robot -> Robot
update (rows, cols) (Robot (px, py) (vx, vy)) = Robot (px', py') (vx, vy)
  where
    px' = (px + vx) `mod` cols
    py' = (py + vy) `mod` rows

position :: Robot -> (Int, Int)
position (Robot p _v) = p

quadrant :: Bounds -> (Int, Int) -> Maybe Int
quadrant (rows, cols) (x, y)
  | x == midCol || y == midRow = Nothing
  | x < midCol && y < midRow = Just 0
  | x > midCol && y < midRow = Just 1
  | x < midCol && y > midRow = Just 2
  | x > midCol && y > midRow = Just 3
  where
    midRow = rows `div` 2
    midCol = cols `div` 2

part1 :: Bounds -> [Robot] -> Int
part1 bounds robots = MS.foldOccur (\_ -> (*)) 1 quadrantBag
  where
    finalRobots = iterate (map $ update bounds) robots !! 100
    quadrants = catMaybes $ map (quadrant bounds . position) finalRobots
    quadrantBag = foldr MS.insert MS.empty quadrants

main = do
  contents <- readFile "input"
  let robots = parse contents
  -- let bounds = (7, 11)
  let bounds = (103, 101)
  print $ part1 bounds robots
