import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.MultiSet as MS

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

visualise :: Bounds -> [Robot] -> String
visualise (rows, cols) robots =
  unlines [[char (x, y) | x <- [0 .. cols - 1]] | y <- [0 .. rows - 1]]
  where
    positions = map position robots
    char pos
      | pos `elem` positions = '#'
      | otherwise = '.'

part2 :: Bounds -> [Robot] -> (Int, String)
part2 bounds =
  (\(i, robots) -> (i, visualise bounds robots))
    . head
    . dropWhile overlapping
    . zip [0 ..]
    . iterate (map $ update bounds)
  where
    overlapping (i, robots) = allUnique $ foldr MS.insert MS.empty (map position robots)
    allUnique bag = MS.size bag /= MS.distinctSize bag

main = do
  contents <- readFile "input"
  let robots = parse contents
  let bounds = (103, 101)
  print $ part1 bounds robots
  let (i, robotString) = part2 bounds robots
  putStrLn robotString
  print i
