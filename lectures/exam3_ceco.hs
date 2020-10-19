import Data.List
main :: IO()
main = do
    --task 2:
  print $ iterator [3, 4, 5] (+1) -- True
  print $ iterator [1, 2, 4] (+1) -- False
  print $ iterator [3] (*2)
    --task 1:
  print (bestStudents [("Иван Иванов", 6.0),("Петър Петров", 5.5),("Мария Маринова", 6.0),("Марина Петрова", 5.0)]) -- →["Иван Иванов", "Мария Маринова"]
  --print (bestStudents [("Ivan Ivanov", 6.0),("Petar Petrov", 5.5),("Maria Marinova", 6.0),("Marina Petrova", 5.0)]) -- →["Ivan Ivanov", "Maria Marinova"]
    --task 3:
  print (f 3) -- → 13 
  print (f 1) -- → 11 
  print (f 8) -- → 0
    --task 2 solution:
iterator :: [Int] -> (Int -> Int) -> Bool
iterator [] _ = error "Invalid input"
iterator [x] f = True
iterator (x:xs) f
  |(head xs == f x) = True && (iterator xs f)
  |otherwise = False
    --task 1 solution:
type Student = (String , Double)
bestStudents :: [Student] -> [String]
bestStudents students = [name | (name, grade) <- students, grade == (maxGrade students)]
maxGrade :: [Student] -> Double
maxGrade students = maximum [grade | (_, grade) <- students]
    --Програмата ми изкарва като изход комбинации от 4-цифрени числа и причината заради това е, че имената са на кирилица.
    
    --task 3 solution:
f :: (Int -> Int)
f = listToFunction [1,2,3]
listToFunction :: [Int] -> (Int -> Int)
listToFunction lst = (\ x -> if elem x lst then x + 10 else 0)
    --task 4 solution:
data BTree a = Empty | Node a (BTree a) (BTree a)
tree :: BTree Int                                  
tree = Node 1 (Node 2 (Node 4 (Node 8 Empty Empty)
                              (Node 9 Empty Empty))                  
                      (Node 5 Empty (Node 10 Empty Empty)))
              (Node 3 (Node 6 Empty (Node 11 Empty Empty))                 
                      (Node 7 Empty (Node 12 Empty Empty)))                 
--singleCousin :: BTree -> [Int]
--singleCousin (Node cur left right) =