import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.Maybe (fromJust, isJust)
import Debug.Trace

data State = State {registers::Registers, pc::Int, input::[Int], output::[Int], waiting::Bool}
    deriving (Eq, Show)
data Op = Snd Arg | Set Arg Arg | Add Arg Arg | Mul Arg Arg | Mod Arg Arg | Rcv Arg | Jump Arg Arg
    deriving (Eq, Show)
data Arg = Register String | Constant Int
    deriving (Eq, Show)
type Registers = M.Map String Int
type Program = [Op]

single = [("snd",Snd),("rcv",Rcv)]
double = [("set",Set),("add",Add),("mul",Mul),("mod",Mod),("jgz",Jump)]

parse :: [String] -> Op
parse [op,arg] = (fromJust $ lookup op single) (parseVal arg)
parse [op,arg1,arg2] = (fromJust $ lookup op double) (parseVal arg1) (parseVal arg2)

parseVal :: String -> Arg
parseVal s = if all isAlpha s then Register s else Constant (read s)

eval :: Registers -> Arg -> Int
eval _ (Constant val) = val
eval r (Register s) = M.findWithDefault 0 s r

step :: Registers -> Op -> Registers
step r (Snd arg) = M.insert "sound" (eval r arg) r
step r (Set arg1@(Register r1) arg2) = M.insert r1 (eval r arg2) r
step r (Add arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 + eval r arg2) r
step r (Mul arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 * eval r arg2) r
step r (Mod arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 `mod` eval r arg2) r
step r (Rcv arg1@(Register r1)) = if eval r arg1 == 0 then r else M.insert "first" 1 $ M.insert r1 (M.findWithDefault 0 "sound" r) r
step r (Jump arg1@(Register r1) arg2) = if eval r arg1 == 0 then r else M.insert "pc" (M.findWithDefault 0 "pc" r + eval r arg2) r

execute :: Registers -> Program -> Registers
execute r program = if pc >= length program || isJust (M.lookup "first" r) then r else execute next program
    where next = if M.findWithDefault 0 "pc" effect == pc then M.insert "pc" (pc+1) effect else effect
          effect = step r (program !! pc)
          pc = M.findWithDefault 0 "pc" r

inc :: State -> State
inc s = s {pc=pc s+1}
wait :: State -> State
wait s = s {waiting=True}
resume :: State -> State
resume s = s {waiting=False}
pop :: State -> State
pop s = s {input=tail $ input s}
set :: State -> String -> Int -> State
set state reg val = state {registers=M.insert reg val (registers state)}
evalS :: State -> Arg -> Int
evalS state = eval (registers state)

step2 :: State -> Op -> State
step2 state (Snd arg) = inc state {output=output state ++ [eval (registers state) arg]}
step2 state (Set arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg2)
step2 state (Add arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg1 + evalS state arg2)
step2 state (Mul arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg1 * evalS state arg2)
step2 state (Mod arg1@(Register r1) arg2) = inc $ set state r1 (evalS state arg1 `mod` evalS state arg2)
step2 state (Rcv arg1@(Register r1)) = if null (input state) then wait state else pop $ inc $ set state r1 $ head (input state)
step2 state (Jump arg1 arg2) = if evalS state arg1 > 0 then state {pc=pc state+evalS state arg2} else inc state

execute2 :: State -> Program -> State
execute2 state program = if pc next > length program || waiting next then next else execute2 next program
    where next = step2 state (program !! pc state)

part2 :: Program -> Int
part2 program = go p0 p1 0
    where p0 = State {registers=M.fromList [("p",0)],pc=0,input=[],output=[],waiting=False}
          p1 = State {registers=M.fromList [("p",1)],pc=0,input=[],output=[],waiting=False}
          done state = pc state > length program || (waiting state && null (input state))
          go one two soFar = if done oneSends && done twoSends then total else continue
            where one' = execute2 one program
                  two' = execute2 two program
                  total = soFar + length (output two')
                  oneSends = one' {input=input one'++output two', output=[]}
                  twoSends = two' {input=input two'++output one', output=[]}
                  continue = go (resume oneSends) (resume twoSends) total

part1 :: Program -> Int
part1 = fromJust . M.lookup "sound" . execute M.empty

main = do
    program <- map (parse . words) . lines <$> readFile "day18.txt"
    print $ part1 program
    print $ part2 program
