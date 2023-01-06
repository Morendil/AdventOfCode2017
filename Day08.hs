import qualified Data.Map as M
import Data.Maybe (fromJust)

type Instruction = [String]
type Registers = M.Map String Int

doCmp :: String -> (Int -> Int -> Bool)
doCmp "==" = (==)
doCmp "!=" = (/=)
doCmp ">=" = (>=)
doCmp "<=" = (<=)
doCmp "<" = (<)
doCmp ">" = (>)
doCmp _ = error "Wat"

execute :: Registers -> Instruction -> Registers
execute regs (reg:op:arg:_:cond:cmp:targ:_) = if condMet then maxified else regs
    where condValue = M.findWithDefault 0 cond regs
          regValue = M.findWithDefault 0 reg regs
          condMet = doCmp cmp condValue (read targ)
          modified = M.insert reg changeReg regs
          changeReg = (if op=="inc" then (+) else (-)) regValue (read arg)
          maxified = M.insert "max" (maximum modified) modified

part1 :: [Instruction] -> Int
part1 = maximum . M.delete "max" . foldl execute M.empty

part2 :: [Instruction] -> Int
part2 = fromJust . M.lookup "max" . foldl execute M.empty

main = do
    program <- map words . lines <$> readFile "day08.txt"
    print $ part1 program
    print $ part2 program
