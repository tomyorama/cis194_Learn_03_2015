{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x)) 

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV from to v = do
  l <- getListV
  return (v // l)
  where 
    getListV = do
      f <- v !? from
      t <- v !? to
      return [(from,t),(to,f)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f l = sequence (map f l)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM (v !?) l 

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = randomValue >>= (\x -> return (v !? x))
  where randomValue = getRandomR (0, (V.length v))

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ mapM (\_ -> getRandom) [1..n]

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n l = liftM V.fromList $ mapM (\_ -> getRandomR l) [1..n]

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = do
  i_j <- indexI_J
  return (foldl (\acc (i, x) -> swapVV i x acc) v i_j)
  where 
    i_jValues = mapM (\i -> getRandomR (0,i)) (reverse [1..((V.length v) - 1)])
    indexI_J = liftM (zip [0..]) i_jValues
    swapVV x y v = case maybeSwap of
      Just v1 -> v1
      Nothing -> V.empty
      where 
        maybeSwap = swapV x y v


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v a = getRes $ V.foldl (\(index, leftList, rightList) x 
                            -> 
                              if index /= a 
                                then (index + 1, listComp leftList x (x < valueA), listComp rightList x (x >= valueA))
                                else (index + 1,leftList, rightList)) (0, [],[]) v
  where
    valueA = v!a
    listComp list x cond
      | cond == True = x:list
      | otherwise = list
    getRes (_, l, r) = (V.fromList l, valueA, V.fromList r)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v 
  | v == V.empty = V.empty
  | otherwise = let 
    x = V.head v
    xs = V.tail v in qsort [ y | y <- xs, y < x ]
                  <> (x `cons` qsort [ y | y <- xs, y >= x ])

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | v == V.empty = return V.empty
  | otherwise = let       
    randonPivot = getRandomR (0, (V.length v - 1))
    in do
      r <- randonPivot
      let (first, pivot, last) = partitionAt v r
      cLeft <- qsortR first
      cRight <- qsortR last
      return (cLeft <> (pivot `cons` cRight))  
     
--test
v = V.fromList $ reverse [1..10000]

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select a v = do
  r <- getRandomR (0, (V.length v - 1))
  let (first, pivot, last) = partitionAt v r
  case (V.length first) `compare` a of
    GT -> select a first
    LT -> select (a - (V.length first) - 1) last
    otherwise -> return $ v !? r 

  


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = do
  allSuits <- suits
  allLabels <- labels
  return (Card allLabels allSuits)

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d = do
  head <- d !? 0
  return (head, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d = foldM (\(cards, deck) f -> do
                          (newCard, deck) <- f deck 
                          return (newCard:cards, deck)) ([], d) (replicate n nextCard)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
