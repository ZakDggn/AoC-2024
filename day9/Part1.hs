import Data.Char (digitToInt)
import Data.Sequence (Seq (Empty, (:<|)), ViewR ((:>)), viewr)
import qualified Data.Sequence as Seq

data BlockType = File | Free

data State = State
  { blockType :: BlockType,
    iD :: Int,
    endID :: Int,
    moved :: Int
  }

parse :: String -> Seq Int
parse = fmap digitToInt . Seq.fromList . head . lines

compact :: State -> Seq Int -> [Int]
compact _ Empty = []
compact (State File iD endID moved) (len :<| diskMap)
  | iD == endID = replicate (len - moved) iD
  | otherwise = replicate len iD ++ compact (State Free (iD + 1) endID moved) diskMap
compact (State Free iD endID moved) (freeLen :<| diskMap) =
  blocks ++ case compare freeLen blocksLeft of
    EQ -> compact (State File iD (endID - 1) 0) diskMap''
    GT -> compact (State Free iD (endID - 1) 0) $ (freeLen - toMove) :<| diskMap''
    LT -> compact (State File iD endID (moved + freeLen)) diskMap
  where
    (diskMap' :> fileLen) = viewr diskMap
    (diskMap'' :> _) = viewr diskMap'
    blocksLeft = fileLen - moved
    toMove = min freeLen blocksLeft
    blocks = replicate toMove endID

part1 :: Seq Int -> Int
part1 diskMap = checksum $ compact (State File 0 endID 0) diskMap
  where
    endID = (Seq.length diskMap) `div` 2
    checksum = sum . zipWith (*) [0 ..]

main = do
  contents <- readFile "input"
  let diskMap = parse contents
  print $ part1 diskMap
