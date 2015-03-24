module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st x i = \z -> if x == z 
                        then i
                        else st z

empty :: State
empty = \_ -> 0 

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state exp = case exp of
                    (Var vr) -> state vr
                    (Val i) -> i
                    (Op e1 bop e2) -> makeOperation bop (evalE state e1) (evalE state e2) 

makeOperation :: Bop -> Int -> Int -> Int
makeOperation opp a b =case opp of
                        Plus ->  a + b
                        Minus -> a - b
                        Times -> (fromIntegral a) * (fromIntegral b)
                        Divide ->  a `div` b 
                        Gt -> castBool ( a > b )
                        Ge -> castBool  (a >= b)
                        Lt -> castBool (a < b)     
                        Le -> castBool ( a <= b)
                        Eql -> castBool (a == b)

castBool :: Bool -> Int
castBool True = 1
castBool False = 0



                  

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar stm = case stm of
        (Assign name exp) -> DAssign name exp 
        (Incr name) -> DAssign name (Op (Var name) Plus (Val 1))
        (If exp st1 st2) -> DIf exp (desugar st1) (desugar st2) 
        (While exp stm) -> DWhile exp (desugar stm)
        (For st1 exp1 st2 st3) -> DSequence (desugar st1) (DWhile exp1 (DSequence (desugar st3) (desugar st2)))
        (Sequence st1 st2) -> DSequence (desugar st1) (desugar st2)




-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state stm = case stm of
                        (DAssign name exp) -> extend state name (evalE state exp)
                        (DIf exp1 st1 st2) -> if (evalE state exp1) > 0
                                                then evalSimple state st1
                                                else evalSimple state st2
                        (DWhile exp1 st1) -> if (evalE state exp1) > 0
                                               then evalSimple (evalSimple state st1) (DWhile exp1 st1) 
                                               else state 
                        (DSequence st1 st2) -> evalSimple (evalSimple state st1) st2
                        (_) -> state




run :: State -> Statement -> State
run state stm = evalSimple state (desugar stm) 

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
