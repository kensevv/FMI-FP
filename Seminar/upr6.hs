import Data.Char

main :: IO()
main = do
    print 6
    print (filterSmallerThan3 [1..10] 5)
    print (filterSmallerThan3 [1.2,1.5,1.7] 1.5)
    print (isAscending 12345)
    print (isAscending 213)
    print (ord 'a')
    print (chr 97)
    print (ord '5' - ord '0')
    print (toList 12345)
    print (zip [1,2] [4,5,6])
    print (check (toList 12345))
    print (isAscending1 12345)
    print (isImage1 [1,2,3] [6,7,8])
    print (isImage1 [1,2,3] [6,8,8])
    print (take 10 [1..])
    print (zip [0..] ['a'..'z'])
    print (chunksOf 3 [1..14])
    print (divisors 1024)
    print (sum (divisors 8128))
    print (isTriangular [[1,2,3],[0,4,5],[0,0,6]])
    print (isTriangular [[1,2,3],[0,4,5],[1,0,6]])

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs k = [x * k | x <- xs]

multiplyAllBy1 :: [Int] -> Int -> [Int]
multiplyAllBy1 xs k = map (* k) xs

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = [x | x <- xs, x >= n]

filterSmallerThan1 :: [Int] -> Int -> [Int]
filterSmallerThan1 xs n = filter (\ x -> x >= n) xs

filterSmallerThan2 :: [Int] -> Int -> [Int]
filterSmallerThan2 xs n = filter (>= n) xs

filterSmallerThan3 :: Ord t => [t] -> t -> [t]
filterSmallerThan3 zs n = [z | z <- zs, z >= n]

-- 1234 -> [1,2,3,4] -> True
-- 312  -> [3,1,2]   -> False
isAscending :: Integer -> Bool
isAscending n = check1 (reverse (helper1 n))

helper1 :: Integer -> [Integer]
helper1 n =
    if n < 10
    then [n]
    else (n `mod` 10) : helper1 (n `div` 10)

check1 :: [Integer] -> Bool
check1 [_] = True
check1 (x:y:xs) = x < y && check1 (y:xs)

-- String == [Char]
toList :: Integer -> [Int]
toList n = [ord d - ord '0' | d <- show n]

check :: [Int] -> Bool
check ds = and [a < b | (a, b) <- zip ds (tail ds)]

isAscending1 :: Integer -> Bool
isAscending1 = check . toList

isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = helper as bs (b - a)
    where
        helper []     []     _ = True
        helper (a:as) (b:bs) x = (b - a) == x && helper as bs x

isImage1 :: [Int] -> [Int] -> Bool
isImage1 (a:as) (b:bs) = and [bi - ai == b - a | (ai, bi) <- zip as bs]

isImage2 :: [Int] -> [Int] -> Bool
isImage2 (a:as) (b:bs) = null [1 | (ai, bi) <- zip as bs, bi - ai /= b - a]

-- take, drop
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k as = take k as : chunksOf k (drop k as)

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n-1], n `mod` d == 0]

{-
    [[1,2,3],   [[1,0,0],   [[0,4,5],
     [0,4,5],    [0,4,0],    [0,4,5],
     [0,0,6]]    [0,0,6]]    [0,4,5]]
-}
isTriangular :: [[Int]] -> Bool
isTriangular []    = True
isTriangular [[_]] = True
isTriangular xss =
    all (== 0) (tail (map head xss)) && isTriangular (tail (map tail xss))