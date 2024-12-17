import Data.Bits (xor)
import Data.List ((!?))
import Data.List.Split (chunksOf, splitOn)

data Registers = Registers {regA :: Int, regB :: Int, regC :: Int} deriving (Show)

type Operand = Int

type Opcode = Int

type Instruction = (Opcode, Operand)

data State = State {registers :: Registers, pointer :: Int, output :: [Int]} deriving (Show)

combo :: Registers -> Operand -> Int
combo registers operand
  | 0 <= operand && operand <= 3 = operand
  | operand == 4 = regA registers
  | operand == 5 = regB registers
  | operand == 6 = regC registers
  | otherwise = error $ "Invalid combo operand: " ++ show operand

setRegA :: Int -> Registers -> Registers
setRegA value (Registers _ b c) = Registers value b c

setRegB :: Int -> Registers -> Registers
setRegB value (Registers a _ c) = Registers a value c

setRegC :: Int -> Registers -> Registers
setRegC value (Registers a b _) = Registers a b value

perform :: Instruction -> State -> State
perform (0, operand) (State registers pointer output) = State registers' (pointer + 1) output
  where
    numerator = regA registers
    denominator = 2 ^ (combo registers operand)
    result = numerator `div` denominator
    registers' = setRegA result registers
perform (1, operand) (State registers pointer output) = State registers' (pointer + 1) output
  where
    result = (regB registers) `xor` operand
    registers' = setRegB result registers
perform (2, operand) (State registers pointer output) = State registers' (pointer + 1) output
  where
    result = (combo registers operand) `mod` 8
    registers' = setRegB result registers
perform (3, operand) (State registers pointer output)
  | regA registers == 0 = State registers (pointer + 1) output
  | otherwise = State registers operand output
perform (4, _) (State registers pointer output) = State registers' (pointer + 1) output
  where
    result = (regB registers) `xor` (regC registers)
    registers' = setRegB result registers
perform (5, operand) (State registers pointer output) = State registers (pointer + 1) output'
  where
    result = (combo registers operand) `mod` 8
    output' = output ++ [result]
perform (6, operand) (State registers pointer output) = State registers' (pointer + 1) output
  where
    numerator = regA registers
    denominator = 2 ^ (combo registers operand)
    result = numerator `div` denominator
    registers' = setRegB result registers
perform (7, operand) (State registers pointer output) = State registers' (pointer + 1) output
  where
    numerator = regA registers
    denominator = 2 ^ (combo registers operand)
    result = numerator `div` denominator
    registers' = setRegC result registers
perform (opcode, _) _ = error $ "Invalid opcode: " ++ show opcode

run :: [Instruction] -> State -> State
run instructions state = case instructions !? (pointer state) of
  Nothing -> state
  Just instruction -> run instructions state'
    where
      state' = perform instruction state

part1 :: Registers -> [Instruction] -> State
part1 registers instructions = run instructions (State registers 0 [])

parse :: String -> (Registers, [Instruction])
parse contents = (Registers a b c, instructions)
  where
    [registerStrings, [program]] = splitOn [""] $ lines contents
    [a, b, c] = map (read . last . words) registerStrings
    programNums = map read . splitOn "," . last . words $ program
    instructions = map tuplify . chunksOf 2 $ programNums
    tuplify [x, y] = (x, y)

main = do
  contents <- readFile "input"
  let (registers, instructions) = parse contents
  let (State _ _ output) = part1 registers instructions
  print output
