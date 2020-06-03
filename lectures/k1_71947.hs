import Data.Char
import Data.List


-- zad1
--checkSequence :: [Int] -> Bool
--checkSequence list = helper [ (x < (head xs)) && ((div (head xs) x) /= 0) | (x:xs) <- list]
     

checkSequence :: [Int] -> Bool
checkSequence [] = True
checkSequence [_] = True 
checkSequence (x:xs) = x < (head xs) && ( (div (head xs) x) /= 0 ) && checkSequence (xs)

--zad2
--removeNb :: Int -> [(Int, Int)]
--removeNb n = [helper2 n 1 n]

--helper2 :: Int -> Int -> Int -> (Int,Int)
--helper2 n a b 
--     | a >= n  = ()
--     | b == 0  = ()
--     |( a*b == ((sum [1 .. n]) -(a+b) )) = (a,b)
--     | otherwise (helper2 n (a+1) (b-1))

main :: IO()
main = do
    --zad1
     print (checkSequence [2, 9, 15]) -- → True
     print (checkSequence [11, 14, 20, 27, 31]) -- → True
     print (checkSequence [11, 14, 28, 27, 31]) -- → False
     print (checkSequence [11, 14, 14, 29, 31]) -- → False
     --zad 2
     --print (removeNb 26) -- → [(15,21),(21,15)]
     --print (removeNb 100) -- → []
     --print (removeNb 101) -- → [(55,91),(91,55)]
