import qualified Data.Map as M
import Data.Maybe (fromJust)

type Action = (Int, Int, String)
type Rule = (String, (Action, Action))
type Tape = (M.Map Int Int, (String, Int))
type Blueprint = (Int, [Rule])

diagnostic :: Tape -> Int
diagnostic = length . filter (==1) . M.elems . fst

turing :: Blueprint -> Tape
turing (steps, rules) = go steps initial
    where initial = (M.empty, ("A",0))
          go 0 state = state
          go n state = go (n-1) (step rules state)

step :: [Rule] -> Tape -> Tape
step rules (tape, (state, pos)) = (M.insert pos value tape, (nextState, pos+offset))
    where currentValue = M.findWithDefault 0 pos tape
          (zeroAction, oneAction) = fromJust $ lookup state rules
          (value, offset, nextState) = if currentValue == 0 then zeroAction else oneAction

main = do
    let sample_blueprint = (6, [("A",((1,1,"B"),(0,-1,"B"))),("B",((1,-1,"A"),(1,1,"A")))])
        input_blueprint = (12629077, [
            ("A",((1,1,"B"),(0,-1,"B"))),
            ("B",((0,1,"C"),(1,-1,"B"))),
            ("C",((1,1,"D"),(0,-1,"A"))),
            ("D",((1,-1,"E"),(1,-1,"F"))),
            ("E",((1,-1,"A"),(0,-1,"D"))),
            ("F",((1,1,"A"),(1,-1,"E")))])
    print $ diagnostic $ turing input_blueprint