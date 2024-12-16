import Data.Array
import Data.List (intercalate)
import Data.List.Split (chunksOf, splitOn)

type Move = Char

type Position = (Int, Int)

type Grid = Array Position Char

parse :: String -> (Grid, Position, [Move])
parse contents = (grid, position, moves)
  where
    [gridString, movesString] = splitOn "\n\n" contents
    gridLines = lines gridString
    maxY = length gridLines - 1
    maxX = length (gridLines !! 0) - 1
    grid = listArray ((0, 0), (maxY, maxX)) $ concat gridLines
    [position] = [(y, x) | y <- [0 .. maxY], x <- [0 .. maxX], grid ! (y, x) == '@']
    moves = filter (/= '\n') movesString

delta :: Move -> Position
delta '^' = (-1, 0)
delta 'v' = (1, 0)
delta '<' = (0, -1)
delta '>' = (0, 1)

nextPos :: Position -> Move -> Position
nextPos (y, x) move = let (dy, dx) = delta move in (y + dy, x + dx)

prevPos :: Position -> Move -> Position
prevPos (y, x) move = let (dy, dx) = delta move in (y - dy, x - dx)

canMove :: Grid -> Position -> Move -> Bool
canMove grid pos move
  | grid ! pos == '.' = True
  | grid ! pos == '#' = False
  | otherwise = canMove grid (nextPos pos move) move

updateList :: Grid -> Position -> Move -> [(Position, Char)]
updateList grid pos move
  | grid ! pos == '.' = [(pos, prev)]
  | grid ! pos == '@' = (pos, '.') : rest
  | otherwise = (pos, prev) : rest
  where
    prev = grid ! (prevPos pos move)
    rest = updateList grid (nextPos pos move) move

update :: Grid -> Position -> Move -> (Grid, Position)
update grid pos move
  | canMove grid pos move = (grid', pos')
  | otherwise = (grid, pos)
  where
    grid' = grid // updateList grid pos move
    pos' = nextPos pos move

run :: Grid -> Position -> [Move] -> Grid
run grid _ [] = grid
run grid pos (move : moves) = run grid' pos' moves
  where
    (grid', pos') = update grid pos move

part1 :: Grid -> Position -> [Move] -> Int
part1 grid pos moves = sum . map (coordinates . fst) $ boxes
  where
    finalGrid = run grid pos moves
    boxes = filter ((== 'O') . snd) $ assocs finalGrid
    coordinates (y, x) = y * 100 + x

visualise :: Grid -> String
visualise grid = intercalate "\n" $ chunksOf (maxX + 1) $ elems grid
  where
    (_, maxX) = snd $ bounds grid

main = do
  contents <- readFile "input"
  let (grid, position, moves) = parse contents
  print $ part1 grid position moves
