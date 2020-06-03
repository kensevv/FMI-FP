import Data.Char

main :: IO()
main = do
    print 7
    print (map (* 2) [1..10])
    print (map (\ x -> chr (x + ord 'a')) [1..10])
    print (filter (\ x -> x `mod` 2 == 0) [1..10])
    print (foldr (+) 0 [1..10])
    print (foldl (+) 0 [1..10])
    print (foldl (flip (:)) [] [1..10])
    print (foldr (:) [] [1..10])
    print (foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" (map show [1..13]))
    print (foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" (map show [1..13]))
    print (primesInRange 1 100)
    print (prodSumDiv [1..10] 2)
    print (isSorted [1,2,3])
    print (isSorted [1,3,2])
    print (isSorted1 [1,2,3])
    print (isSorted1 [1,3,2])
    print (insert 10 [1..5])
    print (insert 1 [11..15])
    print (insert 12 [11..15])
    print (merge [1,3,5] [2,4,6,8])
    print (insertionSort1 [2,7,12,0,34,120,86])

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [k | k <- [a..b], [1, k] == [d | d <- [1..k], k `mod` d == 0]]

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, sum [d | d <- [1..x], x `mod` d == 0] `mod` k == 0]

isSorted :: [Int] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

isSorted1 :: [Int] -> Bool
isSorted1 xs = all (\ (a, b) -> a <= b) (zip xs (tail xs))

isSorted2 :: [Int] -> Bool
isSorted2 xs@(_:ys) = all (\ (a, b) -> a <= b) (zip xs ys)

insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x ys@(z:zs) =
    if x <= z
    then x:ys
    else z:(insert x zs)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge zs@(x:xs) ts@(y:ys)
    | x <= y    = x : merge xs ts
    | otherwise = y : merge zs ys

insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSort1 :: [Int] -> [Int]
insertionSort1 = foldr insert []