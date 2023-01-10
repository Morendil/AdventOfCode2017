import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.Maybe (fromJust, isJust)

data State = State {registers::Registers, pc::Int, muls::Int}
    deriving (Eq, Show)
data Op = Set Arg Arg | Sub Arg Arg | Mul Arg Arg | Jump Arg Arg
    deriving (Eq, Show)
data Arg = Register String | Constant Int
    deriving (Eq, Show)
type Registers = M.Map String Int
type Program = [Op]

double = [("set",Set),("sub",Sub),("mul",Mul),("jnz",Jump)]

parse :: [String] -> Op
parse [op,arg1,arg2] = (fromJust $ lookup op double) (parseVal arg1) (parseVal arg2)

parseVal :: String -> Arg
parseVal s = if all isAlpha s then Register s else Constant (read s)

eval :: Registers -> Arg -> Int
eval _ (Constant val) = val
eval r (Register s) = M.findWithDefault 0 s r

inc :: State -> State
inc s = s {pc=pc s+1}
mul :: State -> State
mul s = s {muls=muls s+1}
set :: State -> String -> Int -> State
set state reg val = state {registers=M.insert reg val (registers state)}
evalS :: State -> Arg -> Int
evalS state = eval (registers state)

step :: State -> Op -> State
step state (Set arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg2)
step state (Sub arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg1 - evalS state arg2)
step state (Mul arg1@(Register r1) arg2) = mul $ inc $ set state r1 (evalS state arg1 * evalS state arg2)
step state (Jump arg1 arg2) = if evalS state arg1 /= 0 then state {pc=pc state+evalS state arg2} else inc state

execute :: State -> Program -> State
execute state program = if pc next >= length program then next else execute next program
    where next = step state (program !! pc state)

part1 :: Program -> Int
part1 = muls . execute State {registers=M.empty,pc=0,muls=0}

main = do
    program <- map (parse . words) . lines <$> readFile "day23.txt"
    print $ part1 program
