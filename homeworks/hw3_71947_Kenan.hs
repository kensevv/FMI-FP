-- Kenan Yusein 71947 HW3

-- Imam bug v reshenieto na 2ra zadacha; 
-- Reshavam 3ta chrez reshenieto na 2ra zatova i na 3ta moje da poluchavam greshen rezultat v sluchay na ekstremen primer :D
-- Zatova mislq che 3ta mi zavisi ot 2ra. Sus sigurnost pri pravilna funkciq na 2ra zadacha 3ta shte mi raboti pravilno 

data BTree a = Empty | Node a (BTree a) (BTree a) 

t1 :: BTree Char                                  --      a
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)    --     / \
                        (Node 'd' Empty Empty))   --    c   b
              (Node 'b' Empty                     --   / \   \
                        (Node 'e' Empty Empty))   --  f   d   e 
                        
t2 :: BTree Char                                        -- a
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)         -- / \
 Empty)                                               -- c   b
 (Node 'b' Empty Empty)                              -- /
                                                    -- d

-- TASK1:
containsWord :: BTree Char -> [Char] -> Bool
containsWord t word = containsHelper (t) (word) (False)

containsHelper :: BTree Char -> [Char] -> Bool -> Bool -- the flag here determines wether the first letter has been already found or not.
containsHelper _ [] _ = False
containsHelper Empty _ _  = False
containsHelper (Node cur (left) (right)) word flag
      | cur == head word && isEmptytree left && isEmptytree right && isEmptyWord (tail word) = True -- If we have reached the end of the tree and checked all letters of the word then its a match
      | cur == head word      = containsHelper (left) (tail word) (True)  || containsHelper (right) (tail word) (True) -- if cur matches the current letter of the word then we Deep search from that point
      | flag == False         = containsHelper (left) (word) (False) || containsHelper (right) (word) (False) -- if we havent found the first letter of the word yet we give the recursion the full word to keep deep searching
      | otherwise             = False -- if the flag is True then the first letter has already been found and cut trought so its not a wrod

isEmptytree :: BTree Char -> Bool -- checks if a tree is empty
isEmptytree Empty = True
isEmptytree _ = False

isEmptyWord :: [Char] -> Bool --
isEmptyWord [] = True
isEmptyWord _ = False

-- TASK2: using isEmptytree function from task 1
genWords :: BTree Char -> [[Char]]
genWords (Node cur (left) (right))
       | isEmptytree left && isEmptytree right              = elemtoList cur
       | not(isEmptytree left) && not(isEmptytree right)    =  (addThem (cur) (genWords right)) ++ (genWords right) ++ (addThem (cur) (genWords left)) ++ (genWords left) 
       | not(isEmptytree right)                             = (addThem(cur) (genWords right)) ++ (genWords right)
       | not(isEmptytree left)                              = (addThem(cur) (genWords left)) ++ (genWords left)

elemtoList :: Char -> [[Char]] -- putting an element to list of lists
elemtoList elem = [[elem]]

plus :: [[Char]] -> [[Char]] -- concatting only the first 2 lists of the list of lists given
plus (x:y:xs) = [x++y] 

addThem :: Char -> [[Char]] -> [[Char]] -- adding single char to list of lists
addThem element list = plus((elemtoList element) ++ list)

-- TASK3: using genWords function from task2
allContain :: [BTree Char] -> [[Char]] 
allContain [] = []
allContain [x] = (genWords x)
allContain trees = allContainHelper trees [[]]

allContainHelper :: [BTree Char] -> [[Char]] -> [[Char]] -- the 'rescue' will save the latest filtered [[Char]] for the use of the recursion END
allContainHelper [] rescue = rescue
allContainHelper [x] rescue = filterWords (genWords x) (rescue) 
allContainHelper (x:y:xs) rescue = filterWords (filterWords (genWords x) (genWords y)) (allContainHelper xs (filterWords (genWords x) (genWords y)))


filterWords :: [[Char]] -> [[Char]] -> [[Char]]
filterWords list1 list2 = [ x | x <- list1, (existInList2 x list2)]

existInList2 :: [Char] -> [[Char]] -> Bool
existInList2 _ [] = False
existInList2 word (x:xs) = 
       if word == x
       then True
       else (existInList2 word xs)

-- TASK4:

main :: IO()
main = do
     -- zad1
     print "Task 1:"
     print (containsWord t1 "a") -- False
     print (containsWord t1 "ab") -- False
     print (containsWord t1 "abe") -- True
     print (containsWord t1 "be") -- True
     print (containsWord t1 "e") -- True
     print (containsWord t1 "ae") -- False
     print (containsWord t1 "ac") -- False
     print (containsWord t1 "acf") -- True
     print (containsWord t1 "acd") -- True
     print (containsWord t1 "af") -- False
     print (containsWord t1 "ad") -- False
     print (containsWord t1 "cf") -- True
     print (containsWord t1 "cd") -- True
     print (containsWord t1 "ac")  -- False
     print (containsWord t1 "ce") -- False
     print (containsWord t1 "cb") -- False
     print (containsWord t1 "db") -- False
     
     --zad2
     print "Task 2:"
     print (genWords t1) -- ["acf","acd","abe","cf","cd","f","d","be","e"]
     print (genWords t2) -- ["ab","b", "acd", "cd", "d"]

     --zad3
     print "Task 3:"
     print (allContain []) -- []
     print (allContain [t2]) -- t2 words (genWords t2)
     print (allContain [t1,t2]) -- ["acd","cd","d"]
     print (allContain [t1,t1,t1,t1,t2,t2,t2,t2]) -- allContain [t1,t1]

     --zad4
     --print (isGraceful t3) -- -> True (|3-1|=2, |5-1|=4, |7-1|=6, |9-1|=8)
     --print (isGraceful t4) -- -> False (|9-7|=2, |5-9|=4, |2-9|=7)
     