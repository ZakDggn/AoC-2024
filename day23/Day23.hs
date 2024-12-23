import Control.Arrow ((&&&))
import Data.List (inits, intercalate, nub)
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

findCluster :: Graph -> Node -> Int -> [[Node]]
findCluster graph a = go a [] []
  where
    go node _ _ 1 = [[node]]
    go node seen cluster n = map (node :) clusters
      where
        adj = Set.toList . (graph !)
        nextNodes = filter (\x -> x `notElem` seen && isInCluster x) $ adj node
        isInCluster x = all (`elem` adj x) cluster
        cluster' = node : cluster
        clusters =
          concat
            [ go next (node : seen ++ seen') cluster' (n - 1)
              | (next, seen') <- zip nextNodes (inits nextNodes)
            ]

part2 :: Graph -> String
part2 graph = intercalate "," . head $ findCluster graph "da" 13

main = do
  contents <- readFile "input"
  let graph = parse contents
  print $ part1 graph
  putStrLn $ part2 graph
