import Data.Bits (xor)
import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> [Int]
parse = map read . lines

next :: Int -> Int
next = evolve (* 2048) . evolve (`div` 32) . evolve (* 64)
  where
    evolve f secret = (`mod` 16777216) . xor secret $ f secret

part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate next)

prices :: Int -> [Int]
prices = map (`mod` 10) . take 2000 . iterate next

changes :: [Int] -> [Int]
changes ps = zipWith (-) (drop 1 ps) ps

seqsOfFour :: [a] -> [(a, a, a, a)]
seqsOfFour (w : x : y : z : rest) = (w, x, y, z) : seqsOfFour (x : y : z : rest)
seqsOfFour _ = []

makeSeqToPrice :: Int -> Map (Int, Int, Int, Int) Int
makeSeqToPrice secret = Map.fromListWith (flip const) $ zip seqs (drop 4 ps)
  where
    ps = prices secret
    seqs = seqsOfFour $ changes ps

part2 :: [Int] -> Int
part2 secrets = maximum seqToTotalPrice
  where
    seqToPrices = map makeSeqToPrice secrets
    seqToTotalPrice = Map.unionsWith (+) seqToPrices

main = do
  secrets <- parse <$> readFile "input"
  print $ part1 secrets
  print $ part2 secrets
