-- Kenan Yusein 71947

-- Task 1

listOfIndexes :: Int -> [Int] -> [Int]
listOfIndexes n list = listofIndexesHelper n list 0 []

listofIndexesHelper :: Int -> [Int] -> Int -> [Int] -> [Int] -- i - брояч за индексите на елементите на списъка.
listofIndexesHelper _ [] _ result = result
listofIndexesHelper n (x:xs) i result =
     if x == n 
     then listofIndexesHelper n xs (i+1) (result ++ [i])
     else listofIndexesHelper n xs (i+1) result
     

-- Task 2

-- а)
digits :: Int -> [Int]
digits 0 = []
digits xs = (digits (xs `div` 10)) ++ [xs `mod` 10]

-- б)
decreasing :: [Int] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) = 
     if(x>y)
     then decreasing (y:xs)
     else False

-- в)
decDigits :: Int -> Bool
decDigits x = decreasing (digits x) -- използвам готовата функция от Б)

-- Task 3

averageFunction :: [(Double -> Double)] -> Double -> Double
averageFunction f x = (sum[g x| g<-f]) / fromIntegral(length f)

-- Task 4

data BTree = Empty | Node Int BTree BTree

t1 :: BTree                                      -- 5
t1 = Node 5 Empty                                 -- \
 (Node 4 (Node 5 Empty Empty)                      -- 4
 (Node 7 Empty Empty))                            -- / \
                                                  -- 5 7


t2 :: BTree                             -- 5
t2 = Node 5 (Node 3 Empty Empty)       -- / \
 (Node 4 (Node 5 Empty Empty)          -- 3  4
 (Node 7 Empty Empty))                 --   / \
                                       --   5 7

isBalanced :: BTree -> Bool 
isBalanced Empty = True
isBalanced (Node _ left right) = (abs (size left - size right)) <= 1 && isBalanced left && isBalanced right


size :: BTree -> Int
size Empty    = 0
size (Node cur l r) = 1 + size l + size r

main :: IO()
main = do
    -- Task 1
     print "Task 1 tests"

     print (listOfIndexes 3 [1,2,3,4,3,5,3,2,1]) --  → [2,4,6]
     print (listOfIndexes 4 [1,2,3,2,1,2,3,2,1]) -- → []
     
     -- Task 2
     print "Task 2 tests"
     -- а)
     print (digits 4321) -- → [4,3,2,1]
     -- б)
     print (decreasing [4,3,2,1]) -- → True
     print (decreasing [4,3,5,1]) -- → False
     print (decreasing [4,3,3,1]) -- → False
     -- в)
     print (decDigits 4321) -- → True
     print (decDigits 4322) -- → False
     print (decDigits 7635) -- → False
     -- Task 3
     print "Task 3 tests"
     print (averageFunction ([(+1),(**0.5),(2**)]) 2) -- → 2.804738
     -- Task 4
     print "Task 4 tests"
     
     print (size t1)
     print (size t2)
     print (isBalanced t1) -- → False
     print (isBalanced t2) -- → True