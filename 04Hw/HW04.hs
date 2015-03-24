{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x  = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a1) (P b1)  = (dropZeros (reverse a1)) == (dropZeros (reverse b1))

dropZeros [] = []
dropZeros (l:ls) = if l == 0 
                   then dropZeros ls 
                   else l:(dropZeros ls)
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a, Enum a) => Show (Poly a) where
    show (P l)= 
                let il = reverse (zip [0..] l)
                in foldl (showing) "" il
                         
showing :: (Num x, Show x, Eq x, Enum x) => String -> (x,x) -> String
showing acc (i,xi) = if xi /= 0
                      then 
                              acc ++ (if (length acc > 0)
                                             then " + "
                                             else "") ++ (show xi) ++ (if i /= 0
                                                                        then "x^" ++ (show i)
                                                                        else "") 
                      else acc

                               

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P a2) = P a2
plus (P a1) (P []) = P a1
plus (P (a1:a1s)) (P (a2:a2s)) = P ((a1 + a2):(retList (plus (P a1s) (P a2s))))
                  where
                    retList (P x) = x 

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P as) (P bs) = foldl (plus) (P []) (map (\(x,i) ->P ((zeros i) ++ (multyOne x bs))) (indexEd))
                where 
                      multyOne a= map (\pn -> pn * a)
                      indexEd= zip as [0..]
                      zeros i = take i (repeat 0)
                             

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate    (P i)  = P (map (\x-> x * (-1)) i)
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) num = foldl (\acc (i,ax) -> acc + (ax * num^i)) 0 ( zip [0..] p ) 

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n x = (iterate deriv x)!!n

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P a1) = P (drop 1 $ map (\(i,x) -> i*x) (zip [0..] a1))   

