main :: IO()
main = do
    print 5
    print ((\ x -> x * 2) 2)
    print ((\ x y -> x * y) 2 5)
    print (f1 2)    -- 4
    print (f3 f1 2) -- 6
    print (f3 (\ x -> x * 3) 2) -- 12
    print ((f4 2) 5) -- 20
    print (f5 5)     -- 20
    print (1 + 2)
    print ((1 +) 2)
    print (map (+ 2) [1,2,3]) -- [3,4,5]
    print (map ((+ 2).(* 3)) [1,2,3]) -- [5,8,11]
    print (map (\ x -> x * 3 + 2) [1,2,3]) -- [5,8,11]
    print (incrementAllBy [1,2,3] 5)
    print (g' 1) -- 18
    print ((derive2 g 1e-6) 1) -- 34
    print ((deriven g 1 1e-6) 1) -- 18
    print ((deriven g 2 1e-6) 1) -- 34
    print ((deriven g 5 1e-2) 1) -- 0
    print (newtonSqrt 612) -- 24.74
    print (newtonSqrt 625) -- 25.00


f1 :: Int -> Int
f1 x = x + 2

f2 :: (Int -> Int)
f2 = \ x -> x + 2

-- f3 :: (((Int -> Int) -> Int) -> Int)
f3 :: (Int -> Int) -> Int -> Int
f3 f x = f (x * 2)

-- f4 :: Int -> Int -> Int
f4 :: Int -> (Int -> Int)
f4 x = \ z -> 2 * x * z
-- f4 x z = 2 * x * z

f5 :: Int -> Int
f5 = f4 2

-- map (+ 2) [1,2,3] -> [3,4,5]
-- -> ((+ 2) 1) : ((+ 2) 2) : ((+ 2) 3) : []
-- [(+ 2) x | x <- [1,2,3]]
map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 f (x:xs) = (f x) : map1 f xs

-- filter (\ x ->  mod x 2 == 1) [1,2,3] -> [1,3]
-- [x | x <- [1,2,3], mod x 2 == 1]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ []     = []
filter1 p (x:xs) =
    if p x then x : filter1 p xs else filter1 p xs

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs z = map (+ z) xs

-- 1. a
myIdentity :: a -> a
myIdentity x = x

-- 1. б
myCompose :: (a -> b) -> (c -> a) -> (c -> b)
myCompose f g = \ x -> f (g x)

myCompose1 :: (a -> b) -> (c -> a) -> c -> b
myCompose1 f g x = f (g x)

-- 1. в
myNegate :: (a -> Bool) -> (a -> Bool)
myNegate p = \ x -> (not (p x))

myNegate1 p x = not (p x)
--myNegate1 p x = (not . p) x
--myNegate1 p   = (not . p)

myNegate2 p = (not . p)

-- 1. г
myCurry :: (a -> b -> c -> d) -> a -> (b -> c -> d)
myCurry f x = f x

-- 2
difference :: (Double -> Double) -> Double -> Double -> Double
difference f a b = (f b) - (f a)

-- 3
-- (\ x -> 2 * x)

-- 4
derive :: (Double -> Double) -> Double -> (Double -> Double)
derive f eps = \ x -> (f (x + eps) - f x) / eps

g :: Double -> Double
g x = 2 * x ** 4 + 5 * x * x

g' :: Double -> Double
g' = derive g 1e-6

-- 5
derive2 :: (Double -> Double) -> Double -> (Double -> Double)
derive2 f eps = derive (derive f eps) eps

-- 6
deriven :: (Double -> Double) -> Int -> Double -> (Double -> Double)
deriven f 0 _   = f 
deriven f n eps = derive (deriven f (n - 1) eps) eps

-- 7
repeated :: (a -> a) -> Int -> (a -> a)
repeated f 0 = \ x -> x
repeated f n = f . (repeated f (n - 1))

repeated1 :: (a -> a) -> Int -> (a -> a)
repeated1 f 0 = \ x -> x
repeated1 f n = \ x -> (f . (repeated f (n - 1))) x

-- 8
newtonSqrt :: Double -> Double
newtonSqrt n = helper 10
    where
        helper cx
            | abs (cx * cx - n) < 1e-4 = cx
            | otherwise = helper (cx - (cx * cx - n) / (2 * cx))