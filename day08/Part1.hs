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

pairAntinodes :: Position -> Position -> [Position]
pairAntinodes a b = [a - aToB, b + aToB]
  where
    aToB = b - a

getAntinodes :: [Position] -> [Position]
getAntinodes = concatMap go . tails
  where
    go [] = []
    go [_] = []
    go (p : ps) = concatMap (pairAntinodes p) ps

part1 :: Bounds -> AntenaMap -> Int
part1 (Bounds rows cols) =
  length . nub . filter inBounds . concatMap getAntinodes . Map.elems
  where
    inBounds (V2 x y) = 0 <= x && x < cols && 0 <= y && y < rows

main = do
  contents <- readFile "input"
  let (bounds, antenas) = parse contents
  print $ part1 bounds antenas
