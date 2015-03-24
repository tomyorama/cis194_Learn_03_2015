import System.Environment   
import Data.List

main = do
	--args <- getArgs
	--print (head args)
	--print (doubleEveryOther [8,7,6,5])
	--print (toDigits (read(head args)))
	--print (toDigitsRev (read(head args)))
	--print (sumDigits [8,7,6,5])
	print (validate 4012888888881881)
	print (validate 4012888888881882)


toDigitsRev::Integral x => x -> [x]

toDigitsRev x
	|x>0 = (x `mod` 10):toDigitsRev(x `div` 10)
	|otherwise = []

toDigits = reverse.toDigitsRev

--Fun
doubleEveryOther :: Integral x => [x] -> [x]
doubleEveryOther = reverse.double2nd.reverse

double2nd (x:y:xs) = x : 2 * y : double2nd xs
double2nd a = a

sumDigits x = sum (map (sum.toDigits) x) 

validate :: Integer -> Bool
validate x = (((sumDigits.doubleEveryOther.toDigits) x) `mod` 10)==0


