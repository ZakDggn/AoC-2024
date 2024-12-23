import Control.Arrow ((&&&))
import Data.List (nub)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Node = String

type Edge = (Node, Node)

type Graph = Map Node (Set Node)

makeGraph :: [Edge] -> Graph
makeGraph = foldr insertBoth Map.empty
  where
    insertBoth (a, b) = insert (a, b) . insert (b, a)
    insert (a, b) = Map.insertWith Set.union a (Set.singleton b)

parse :: String -> Graph
parse = makeGraph . map (take 2 &&& drop 3) . lines

findThrees :: Graph -> Node -> [Set Node]
findThrees graph a = map Set.fromList threes
  where
    threes = [[a, b, c] | b <- adj a, c <- adj b, a `elem` adj c]
    adj = Set.toList . (graph !)

part1 :: Graph -> Int
part1 graph = length allThrees
  where
    tNodes = filter ((== 't') . head) $ Map.keys graph
    allThrees = nub $ concatMap (findThrees graph) tNodes

main = do
  contents <- readFile "input"
  let graph = parse contents
  print $ part1 graph
