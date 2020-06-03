main :: IO()
main = do
    print "Task 1"
    print (findSum 0 2 10) -- 3578
    print (findSum 5 3 5)  -- 174
    print (findSum 0 1 4)  -- 25
    print "Task 2"
    print (isSquare 1)    -- True
    print (isSquare 2)    -- False
    print (isSquare 4)    -- True
    print (isSquare 17)   -- False
    print (isSquare 256)  -- True
    print (isSquare 2500) -- True
    print "Task 3"
    print (isSpecial 131 2)   -- True
    print (isSpecial 472 2)   -- False
    print (isSpecial 17197 2) -- True
    print (isSpecial 12234 3) -- False
    print (isSpecial 10113 3) -- True
    
-- Задача 1: 
findSum :: Int -> Int -> Int -> Int
findSum a b n = helper 1 (a + b)
    where
        helper counter acc
            | counter + 2 == n = 3*acc + (2^n)*b
            | otherwise        = helper (counter + 1) (acc + 2^(counter)*b)

-- Задача 2:
isSquare :: Int -> Bool
isSquare p = helper 1
    where
        helper current =
            if (current * current > p)
            then False
            else (current * current == p) || (helper (current + 1))

-- Задача 3:
isSpecial :: Int -> Int -> Bool
isSpecial num k =
    if num < 10^(k-1)
    then True
    else (isPrime (num `mod` 10^k)) && (isSpecial (num `div` 10) k) 

isPrime :: Int -> Bool
isPrime n = null [d | d <- [2..n-1], n `mod` d == 0]