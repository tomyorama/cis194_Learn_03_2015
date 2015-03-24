--------------------------------------------------------------------------------
-- | 
-- Module      : Dice testing
-- Note        : Try maximum number of dices 5
-- 
-- Probability getting wanted combinations after rolling N-dices
-- 
--------------------------------------------------------------------------------

import System.Environment   
import Data.List
import Text.Printf


main = do
    --args <- getArgs
    print "Number of dices:"
    numOfDices <- getLine
    let sampleSpace =  makeSampleSpace (read numOfDices)
        sL = length sampleSpace
        nScales = length (filter isScale sampleSpace)
        n3kind = length (filter is3kind sampleSpace) 
        n4kind = length (filter is4kind sampleSpace) 
        n5kind = length (filter is5kind sampleSpace) 
        nFull = length (filter isFullHouse sampleSpace) 
    print "Probability scales :"
    printf "%.2f%%\n" ( (fromIntegral nScales) / (fromIntegral sL)*100 :: Float)
    print "Probability 3kinds :"
    printf "%.2f%%\n" ( (fromIntegral n3kind) / (fromIntegral sL)*100 :: Float)
    print "Probability full house "
    printf "%.2f%%\n" ( (fromIntegral nFull) / (fromIntegral sL)*100 :: Float)
    print "Probability 4kinds "
    printf "%.2f%%\n" ( (fromIntegral n4kind) / (fromIntegral sL)*100 :: Float)
    print "Probability 5kinds :"
    printf "%.2f%%\n" ( (fromIntegral n5kind) / (fromIntegral sL)*100 :: Float)
    --print (length sampleSpace)
    print (isNkind 2 [1,2,2,2,2])


--------------------------------------------------------------------------------
--  Condition sequences

is2kind = isNkind 2
is4kind = isNkind 4  
is3kind = isNkind 3 
is5kind = isNkind 5
isScale = isNkind 1
isScale2 xs 
        | xs `elem` ( permutations [1..(length xs)]) = True
        | xs `elem` ( permutations [1..(length xs)]) = True
        | otherwise = False
isFullHouse xs = my2kind && my3kind
    where
      allL = map (\x-> count x xs) xs
      my2kind = 2 `elem` allL
      my3kind = 3 `elem` allL
count x = length . filter (\x' -> x' == x)

isNkind ::  Int -> [Int] -> Bool
isNkind n xs = (==n) $ maximum $ map (\x ->count x xs) xs    


--------------------------------------------------------------------------------
--  Sample space



makeSampleSpace n = last $ take (n+1) $ iterate makeRoll [[]]



makeRoll xs = [ y :  x  | x<- xs , y <- [1,2,3,4,5,6]]


