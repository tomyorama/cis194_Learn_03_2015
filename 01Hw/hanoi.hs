import System.Environment   
import Data.List

main = do
    args <- getArgs
    print (hanoi 5 "a" "b" "c")

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [] 
hanoi n from help to = (hanoi (n-1) from to help) ++ [(from,to)] ++ (hanoi (n-1) help from to)

