-- Kenan Yusein 71947 K2

-- Task 1

rotate :: Int -> [Char] -> [Char]
rotate n list = 
      if n > 0
      then rotateAntiClockwise list n list
      else rotateClockwise list n list


rotateClockwise :: [Char] -> Int -> [Char] -> [Char]
rotateClockwise result n list = 
      if n == 0
      then result
      else rotateClockwise ([last list] ++ (init list)) (n+1) ([last list] ++ (init list))

rotateAntiClockwise :: [Char] -> Int -> [Char] -> [Char]
rotateAntiClockwise result n (x:xs) = 
      if n == 0
      then result
      else rotateAntiClockwise (xs ++ [x]) (n-1) (xs ++ [x])

-- Task 2
data BTree = Empty | Node Int BTree BTree

t3 :: BTree -- 1
t3 = Node 1 (Node 2 Empty Empty) -- / \
 (Node 3 Empty Empty) -- 2 3

t4 :: BTree -- 1
t4 = Node 1 (Node 2 (Node 3 Empty Empty) -- / \
 Empty) -- 2 2
 (Node 2 Empty -- / \
 (Node 3 Empty Empty)) -- 3 3

t5 :: BTree -- 1
t5 = Node 1 (Node 2 (Node 3 Empty Empty) -- / \
 (Node 7 (Node 5 Empty Empty) -- 2 2
 Empty)) -- / \ / \
 (Node 2 (Node 7 Empty -- 3 7 7 3
 (Node 5 Empty Empty)) -- / \
 (Node 3 Empty Empty)) -- 5 5

isSymmetric :: BTree -> Bool
isSymmetric tree = isMirror tree tree -- we check if the tree is mirror to itself


isMirror :: BTree -> BTree -> Bool
isMirror Empty Empty = True
isMirror (Node v1 lt1 rt1) (Node v2 lt2 rt2) =
      if v1 == v2
      then (isMirror lt1 rt2) && (isMirror rt1 lt2)
      else False


-- Task 3


--data NestedList = Elem Int | List [NestedList]
--flatten :: NestedList -> [Int]
--flatten nestedlist = [ elem : [] | elem <- nestedlist]

main :: IO()
main = do
    -- task 1
      print (rotate 3 ['a','b','c','d','e','f','g','h']) -- → "defghabc"
      print (rotate (-2) ['a','b','c','d','e','f','g','h']) -- → "ghabcdef"
    
    -- task 2
      print (isSymmetric t3) -- → False
      print (isSymmetric t4) -- → True
      print (isSymmetric t5) -- → True

      -- task 3
      -- print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) -- → [1,2,3,4,5]
      -- print (flatten (Elem 1)) -- → [1])