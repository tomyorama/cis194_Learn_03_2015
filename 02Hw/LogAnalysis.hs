{-# OPTIONS_GHC -Wall #-}
import Log


main = do
    contents <- readFile "error.log" 
    --tooo <- (testParse parse 10 "error.log")
    --print tooo
    --print " ---messages----"
    --mapM_ print (take 30(parse contents))
    --print "---Ordered messages----"
    --mapM_ print  (inOrder (build (take 100(parse contents))))
    print  "---Ordered ERROR messages----"
    mapM_ print (whatWentWrong ((parse contents)))



parseMessage :: String -> LogMessage
parseMessage x =
    let elems = drop 1 $ words x
        in case x of
            ('E':' ':_) -> LogMessage 
              (Error $ read $ head elems)
              (read $ elems !! 1)
              (unwords $ drop 2 elems)  
            ('W':' ':_) -> LogMessage
              Warning
              (read $ head elems)
              (unwords $ drop 1 elems)
            ('I':' ':_) -> LogMessage
              Info
              (read $ head elems)
              (unwords $ drop 1 elems)
            _ -> Unknown x
             
parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree              = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert m Leaf                        = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l n@(LogMessage _ nt _) r)
    | t < nt    = Node (insert m l) n r
    | otherwise = Node l n (insert m r)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node smallertree  msg biggertree) = (inOrder smallertree)++[msg]++(inOrder biggertree) 

isErrorSeverity50 :: LogMessage -> Bool
isErrorSeverity50 (LogMessage (Error s) _ _) = s >= 50
isErrorSeverity50 _                          = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage (Unknown m)        = m

-- testWhatWentWrong parse whatWentWrong "sample.log"
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . (filter isErrorSeverity50)
