
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Data.Bits (xor)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List as DL
import Parser

--files
f_dogOrg = "clues/dog-original.jpg"  
f_dog = "clues/dog.jpg"
f_vic = "clues/victims.json"
f_trans = "clues/transactions.json"
f_newId = "clues/new-ids.json"
f_newTs = "clues/new-transactions.json" 
f_secret= "Haskell Is Great!"
--testing
secret = getSecret f_dogOrg f_dog
decrypt_file = decryptWithKey (C.pack f_secret) f_vic
parseTest = parseFile f_vic :: IO (Maybe [TId])
parseTest1 = parseFile f_trans :: IO (Maybe [Transaction])
getBadTest = getBadTs f_vic f_trans
getFlowTest = do
    t <- getBadTest
    return (fmap getFlow t)
getCriminalTest = do
    t <- getBadTest
    return (fmap (getCriminal . getFlow) t)

undoTest = do
    b <- getFlowTest
    return (fmap (\x -> undoTs x (repeat "")) b) 
-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret forg fcod = do
    orgFile <- BS.readFile forg
    codeFile <- BS.readFile fcod
    let xorFile = BS.zipWith xor (orgFile) (codeFile)
    return (BS.pack $ filter (\x-> not(x == 0)) xorFile)
-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey s f = do 
    encodedF <- BS.readFile (f ++ ".enc")                    
    BS.writeFile f (BS.pack (contents encodedF))
    where
      repKey = BS.concat $ map (\_-> s) [1..] 
      contents = BS.zipWith xor repKey 


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f =  do
    content <- BS.readFile f
    return (decode content)

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fvict ftran = do
    v <- victims
    t <- transactions
    return (fmap (\x -> filter (isElement v) x) t)
    where
      victims = parseFile fvict :: IO (Maybe [TId])
      transactions = parseFile ftran :: IO (Maybe [Transaction])
      isElement (Just vs) tx = (tid tx) `elem` vs
      isElement Nothing _  = False

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow l = Map.fromList (concat $ map (\x-> [(from x, amount x),(to x, (amount x)*(-1))]) l )

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal l = fst (Map.foldlWithKey maxiMap (("Nouser", 3)) l)
                                where
                                  maxiMap acc@(nm,am) x a = 
                                      if am > a then acc
                                      else (x,a)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m li = map (\(py, pee) -> Transaction {from = fst py,to = fst pee, amount= ((snd py) - (snd pee)), tid =""} ) (zip payers payees)
    where
      payers = DL.sortBy (\a b -> snd a `compare` snd b)  $ Map.toList (Map.filter (>0) m)
      payees = DL.sortBy (\a b -> snd a `compare` snd b)  $ Map.toList (Map.filter (<0) m)


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f x = do
    BS.writeFile f (encode x)  

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything f_dogOrg 
                        f_dog
                        f_trans
                        f_vic
                        f_newId
                        f_newTs
  putStrLn crim

