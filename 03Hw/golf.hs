--------------------------------------------------------------------------------
-- | 
-- Module      : Golf homework
-- Note        : lerning
-- 
-- Lerning for haskell - homework 3
-- 
--------------------------------------------------------------------------------

main = do
    print "Testing"
    print (skips [1,2,3,4,5,6])
    print (localMaxima [1,2,3,3,4,3,4,2])


skips :: [a] -> [[a]]
skips [] = [[]]
skips xs = map (\x-> everynth x xs) [1..(length xs)]

everynth :: Int -> [x] -> [x]
everynth n = (map snd) . (filter (\(i,_)->i `mod` n == 0 )) . (zip [1..])


localMaxima2 :: [Integer] -> [Integer]
localMaxima2 (a:b:c:xs) 
          |b > a && b > c = b:(localMaxima (b:c:xs))
          |otherwise = localMaxima (b:c:xs)
localMaxima2 _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(x, _, _) -> x) $
    filter
            (\(x1, x2, x3) -> x1 > x2 && x1 > x3)
                    (zip3 (tail xs) xs (drop 2 xs))


histogram :: [Integer] -> String
histogram xs =
        let freq = map (\x -> length $ filter (x ==) xs) [0..9] in
              fst (foldr
                          (\_ (res, x) -> (showLevel x ++ "\n" ++ res, map (subtract 1) x))
                                  ("", freq)
                                          [1..maximum freq])
                                              ++ "==========\n0123456789\n"

showLevel :: [Int] -> String
showLevel = map (\x -> if x > 0 then '*' else ' ')
