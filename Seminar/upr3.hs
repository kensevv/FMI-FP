main :: IO()
main = do
  print (pow 2 10)              -- 1024.0
  print (isPrime 1)             -- False
  print (isPrime 2)             -- True
  print (isPrime 12)            -- False
  print (isPrime 23)            -- True
  print (isAscending 123)       -- True
  print (isAscending 323)       -- False
  print (countOccurences 0 100) -- 2
  print (countOccurences 2 211) -- 1
  print (isPerfectNumber 28)    -- True
  print (isPerfectNumber 29)    -- False
  print (isPerfectNumber 8128)  -- True
  print (sumPrimeDivisors 8128) -- 129
  print (sumPrimeDivisors 1203) -- 404

pow :: Double -> Int -> Double
pow _ 0 = 1
pow x n = x * pow x (n - 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
  where
    helper d
      | d == n         = True
      | n `mod` d == 0 = False
      | otherwise      = helper (d + 1)

isAscending :: Int -> Bool
isAscending n
  | n < 10                             = True
  | (n `div` 10) `mod` 10 > n `mod` 10 = False
  | otherwise                          = isAscending (n `div` 10)

isAscending1 :: Int -> Bool
isAscending1 n =
  n < 10 || (((n `div` 10) `mod` 10 <= n `mod` 10) && isAscending (n `div` 10))

countOccurences :: Int -> Int -> Int
countOccurences d n
  | n < 10          = if d == n then 1 else 0
  | n `mod` 10 == d = 1 + countOccurences d (n `div` 10)
  | otherwise       = countOccurences d (n `div` 10)

countOccurencesIter :: Int -> Int -> Int
countOccurencesIter d n = helper 0 n
  where
    helper count k
      | k < 10    = (if d == k then 1 else 0) + count
      | otherwise = helper ((if k `mod` 10 == d then 1 else 0) + count) (k `div` 10)

isPerfectNumber :: Int -> Bool
isPerfectNumber n = n == sumDivisors n
  where
    sumDivisors k = helper 2 1
      where
        helper d sum
          | d == k         = sum
          | k `mod` d == 0 = helper (d + 1) (sum + d)
          | otherwise      = helper (d + 1) sum

sumPrimeDivisors :: Int -> Int
sumPrimeDivisors n = helper 2 0
  where
    helper d sum
      | d == n                      = sum
      | n `mod` d == 0 && isPrime d = helper (d + 1) (sum + d)
      | otherwise                   = helper (d + 1) sum