{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches code guess = 
    let 
        zipped = filter (\(x,y)-> x==y) $ zip code guess
        in length zipped
 
-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\x -> length $ filter (==x) code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ map (\(x,y)-> if x < y 
                                then x
                                else y) $ zip (countColors c1) (countColors c2) 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c1 c2 = 
    let exactMatchesL = exactMatches c1 c2 
        matchesL = matches c1 c2 
        in Move c2 exactMatchesL (abs $ exactMatchesL - matchesL)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c1 cn mn) c2 = cn == xn && mn == xm
            where
              (Move _ xn xm) = getMove c1 c2


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n  = last $ take (n+1) $  iterate makeCoodeN [[]]

makeCoodeN :: [[Peg]] -> [[Peg]]
makeCoodeN x = [yx:xx | xx <- x , yx <- colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = solveCodes c (allCodes $ length c)

solveCodes :: Code -> [Code] -> [Move]
solveCodes _ [] = []
solveCodes c (xc:xx) = nextMove : (solveCodes c (filter (isConsistent nextMove) xx))
    where
      nextMove = getMove c xc

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
