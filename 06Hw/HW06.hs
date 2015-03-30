{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor
import qualified System.Random as R

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n  
    | n > 0 = fib (n-1) + fib (n-2)
    | otherwise = 0

fibs1 :: [Integer]
fibs1 = map fib [0..] 

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : (doit fibs2)
      where
        doit allf@(x:y:_) = (x + y) : doit (tail allf)

             

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a str) = a : (streamToList str)  

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a s) = Cons (f a) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x)) 

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a1 s1) s2 = Cons a1 (Cons (getFirst s2) (sInterleave s1 (getSnd s2)))
        where getFirst (Cons a _) = a
              getSnd (Cons _ s) = s

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList 

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = fmap countDivs natsNoZero 
        where
          countDivs n = toInteger ((length $ takeWhile (>0) $ iterate (\x -> if even x then (x `div` 2) else 0) n) -1)
          natsNoZero = sIterate (+1) 1



-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand n = str rs
    where
      r = R.random (R.mkStdGen n) :: (Int, R.StdGen)
      rs = R.randoms (R.mkStdGen n) :: [Int]
      str (x:xs) = Cons x (str xs)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 80 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax l = Just (mm l (0,0))
          where 
                minMaxA (mina, maxa) a = mina `seq` maxa `seq`  ((if a < mina then a else mina),(if a > maxa then a else maxa))
                mm (x:xs) mi = mi `seq` mm xs (minMaxA mi x)


main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
data Matrix = M [[Integer]] deriving (Eq, Show, Read)

instance Num Matrix where
    (+) = undefined 
    (*) (M f) (M s) = M (map (\x -> map (\y -> mmR x y) (transpose s) ) f)
        where
              mR [] _ = []
              mR _ [] = []
              mR (x:xs) (y:ys) = (x*y):(mR xs ys)
              mmR a b = sum (mR a b) 
              
      
    negate = undefined
    fromInteger = undefined
    abs = undefined
    signum = undefined

fastFib :: Int -> Integer
fastFib n = get $ m^n 
    where 
          m = M [[1,1],[1,0]]
          get (M a) = (a!!0)!!1

