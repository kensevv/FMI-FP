-- Kenan Yusein 71947 grupa 2: HW 2
import Data.Char
import Data.List

--Zadacha 1
-- ne razbrah kak da zakruglq Double do 2 cifri sled zapetaykata (0.00)
generate :: Double -> Double -> [Double]
generate p n = 
      (helperGenerate p n 1 [])

helperGenerate :: Double -> Double -> Double -> [Double] -> [Double]
helperGenerate p n j res =
      if j == n + 1
      then res
      else (helperGenerate p n (j+1) (res ++ [(currentEl p j 0 0)]))

currentEl :: Double -> Double -> Double -> Double -> Double
currentEl _ 0 _ _      = 0
currentEl p n k res    =
      if k == n
      then res
      else (currentEl p n (k+1) (res + (1 / (k+1)**p )))

--Zadacha 2
listSquares :: Int -> Int -> [(Int, Int)]
listSquares a b =
      if a == (b+1)
      then []
      else if (isSquare a)
           then (a, (aDividors a 1 0)) : (listSquares (a+1) b)
           else (listSquares (a+1) b)

aDividors :: Int -> Int -> Int -> Int
aDividors m k sum =
      if k > m
      then sum
      else if (m `mod` k == 0)
           then (aDividors m (k+1) (sum + (m `div` k)^2))
           else (aDividors m (k+1) sum)

--Dali e kvadrat chisloto
isSquare :: Int -> Bool
isSquare x = (helperFunc x 1)

helperFunc :: Int -> Int -> Bool 
helperFunc x y =
     if y == ( x `div` 2 ) + 2 -- Duno: maksimalnoto namalenie na broq puti izpulnenie na rekursiqta za koqto se setih 
     then False 
     else (y*y == x) || (helperFunc x (y+1)) -- proverqvam kvadrata na chislata ot 2 do namirane na suvpadenie s n


-- Zadacha 3
type Point = (Double, Double)
splitPoints :: Point -> Double -> [Point] -> ([Point], [Point]) 
splitPoints (x,y) r ps =  (filter pIsInCircle ps, filter (not.pIsInCircle) ps)
    where
    pIsInCircle :: Point -> Bool
    pIsInCircle (a,b) 
            |(dBetweenPs (a,b) (x,y) < r) = True
            |(dBetweenPs (a,b) (x,y) >= r) = False
    dBetweenPs :: Point -> Point -> Double
    dBetweenPs (c,d) (m,n) = (sqrt ( ((c-m)**2) + ((d-n)**2) ))

-- Zadacha 4
type Account = (Int, Int, Double)
type Person = (Int, String, String)

--getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double


main :: IO()
main = do
     --Zadacha 1:
 
      print (generate 1 3)   -- → [1.0,1.5,1.8333333333333333]
      print (generate 0.1 5) -- → [1.0,1.93,2.83,3.70,4.55]

     --Zadacha 2
      print (listSquares 250 300)
      print (listSquares 1 30)

      --Zadacha 3
      print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) 
          --([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])
      print (splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) 
          --([(10.0,15.0),(12.0,14.0)],[(1.0,2.0),(2.0,3.0),(-1.0,1.0)])
      
      --Zadacha 4

      --  ps :: [Person]
      --  ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]

      --  as :: [Account]
      --  as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

       --print (getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas")) -- →24.95
       --print (getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P')) -- → 18.85