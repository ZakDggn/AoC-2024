import Data.Bits (xor)

parse :: String -> [Int]
parse = map read . lines

next :: Int -> Int
next = evolve (* 2048) . evolve (`div` 32) . evolve (* 64)
  where
    evolve f secret = (`mod` 16777216) . xor secret $ f secret

part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate next)

main = do
  secrets <- parse <$> readFile "input"
  print $ part1 secrets
