import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq, Seq(Empty), Seq((:<|)), (|>))
import Data.Sequence qualified as Seq

type Wire = String

data GateType = AND | OR | XOR deriving (Read)

type Gate = (GateType, Wire, Wire)

type State = Map Wire Bool

type Circuit = Map Wire Gate

type Queue = Seq Wire

parse :: String -> (State, Circuit)
parse contents = (state, circuit)
  where
    [stateStrings, circuitStrings] = splitOn [""] $ lines contents
    stateList = map (words . filter (/= ':')) stateStrings
    state = Map.fromList [(wire, toBool value) | [wire, value] <- stateList]
    toBool "0" = False
    toBool _ = True
    circuitList = map (splitOn " -> ") circuitStrings
    circuit = Map.fromList [(wire, parseGate gate) | [gate, wire] <- circuitList]
    parseGate gate = (read gateType, wire1, wire2)
      where
        [wire1, gateType, wire2] = words gate

calcValue :: State -> Circuit -> Wire -> Maybe Bool
calcValue state circuit wire = do
  (gateType, wire1, wire2) <- Map.lookup wire circuit
  let f = case gateType of
        AND -> (&&)
        OR -> (||)
        XOR -> (/=)
  input1 <- Map.lookup wire1 state
  input2 <- Map.lookup wire2 state
  return $ input1 `f` input2

solve :: State -> Circuit -> Queue -> State
solve state _ Empty = state
solve state circuit (wire :<| queue) = case calcValue state circuit wire of
  Nothing -> solve state circuit (queue |> wire)
  Just value -> solve state' circuit queue
    where
      state' = Map.insert wire value state

part1 :: State -> Circuit -> Int
part1 state circuit = Map.foldrWithKey updateDecimal 0 finalState
  where
    finalState = solve state circuit $ Seq.fromList $ Map.keys circuit
    updateDecimal wire value decimal
      | head wire == 'z' && value = decimal + 2 ^ (read $ tail wire)
      | otherwise = decimal

main = do
  contents <- readFile "input"
  let (state, circuit) = parse contents
  print $ part1 state circuit
