import Control.Monad (guard)
import Data.List (nub, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear.V2

data Bounds = Bounds Int Int

type Position = V2 Int

type AntenaMap = Map Char [Position]

parse :: String -> (Bounds, AntenaMap)
parse contents = (Bounds rows cols, antenas)
  where
    grid = lines contents
    rows = length grid
    cols = length (grid !! 0)
    antenas = Map.fromListWith (++) $ do
      y <- [0 .. rows - 1]
      x <- [0 .. cols - 1]
      let char = grid !! y !! x
      guard (char /= '.')
      return (char, [V2 x y])

pairAntinodes :: Bounds -> Position -> Position -> [Position]
pairAntinodes (Bounds rows cols) a b = aSide ++ bSide
  where
    aSide = takeWhile inBounds $ iterate (+ (a - b)) a
    bSide = takeWhile inBounds $ iterate (+ (b - a)) b
    inBounds (V2 x y) = 0 <= x && x < cols && 0 <= y && y < rows

getAntinodes :: Bounds -> [Position] -> [Position]
getAntinodes bounds = concatMap go . tails
  where
    go [] = []
    go [_] = []
    go (p : ps) = concatMap (pairAntinodes bounds p) ps

part2 :: Bounds -> AntenaMap -> Int
part2 bounds = length . nub . concatMap (getAntinodes bounds) . Map.elems

main = do
  contents <- readFile "input"
  let (bounds, antenas) = parse contents
  print $ part2 bounds antenas
