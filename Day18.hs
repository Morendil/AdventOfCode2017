import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.Maybe (fromJust, isJust)

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

execute' :: Registers -> Op -> Registers
execute' r (Snd arg) = M.insert "sound" (eval r arg) r
execute' r (Set arg1@(Register r1) arg2) = M.insert r1 (eval r arg2) r
execute' r (Add arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 + eval r arg2) r
execute' r (Mul arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 * eval r arg2) r
execute' r (Mod arg1@(Register r1) arg2) = M.insert r1 (eval r arg1 `mod` eval r arg2) r
execute' r (Rcv arg1@(Register r1)) = if eval r arg1 == 0 then r else M.insert "first" 1 $ M.insert r1 (M.findWithDefault 0 "sound" r) r
execute' r (Jump arg1@(Register r1) arg2) = if eval r arg1 == 0 then r else M.insert "pc" (M.findWithDefault 0 "pc" r + eval r arg2) r

execute :: Registers -> Program -> Registers
execute r program = if pc > length program || isJust (M.lookup "first" r) then r else execute next program
    where next = if M.findWithDefault 0 "pc" effect == pc then M.insert "pc" (pc+1) effect else effect
          effect = execute' r (program !! pc)
          pc = M.findWithDefault 0 "pc" r

part1 :: Program -> Int
part1 = fromJust . M.lookup "sound" . execute M.empty

main = do
    program <- map (parse . words) . lines <$> readFile "day18.txt"
    print $ part1 program
