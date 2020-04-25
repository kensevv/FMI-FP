-- Kenan Yusein 71947 Grupa 2, HW1

--Zadacha 1
findSum :: Int -> Int -> Int -> Int
findSum a b n =
     if n < 4 -- Proverka za uslovieto n>3
     then error "N must be higher than 3"
     else (helperSum b 0 n) + (helperSum b 0 (n-1)) + (helperSum b 0 (n-2)) + 3*a -- namirame sumite na poslednite 3 i dobavqme 3*a zaradi redicata

helperSum :: Int -> Int -> Int -> Int -- Pomoshtna funkciq za namirane na sumata 2^k * b kato k e ot 0 do n-1
helperSum b k n = 
     if n == k
     then 0 
     else 2^k * b + (helperSum b (k+1) n)

--Zadacha 2
isSquare :: Int -> Bool
isSquare x = (helperFunc x 1)

helperFunc :: Int -> Int -> Bool 
helperFunc x y =
     if y == ( x `div` 2 ) + 2 -- Duno: maksimalnoto namalenie na broq puti izpulnenie na rekursiqta za koqto se setih 
     then False 
     else (y*y == x) || (helperFunc x (y+1)) -- proverqvam kvadrata na chislata ot 2 do namirane na suvpadenie s n
     
--Zadacha 3  
isSpecial :: Int -> Int -> Bool
isSpecial n k =
     if n < 11
     then True
     else isPrime ( (kDigits 0 n k 0) ) && (isSpecial (n `div` 10) k) -- ...

-- Kato trqbva result = 0 i y = 0 
kDigits :: Int -> Int -> Int -> Int -> Int -- Vzimam K-cifrenoto chislo otzad napred ot cifrite na n
kDigits result n k y = 
     if y == k
     then result
     else (kDigits (result + (lastDigit n) * (10^y) ) (n`div`10) k (y+1))
     

lastDigit :: Int -> Int -- pomoshtna fuknciq za vzimane na poslednata cifra na n
lastDigit n = 
     (n `mod` 10)
          

isPrime :: Int -> Bool
isPrime n =
   if n == 1
   then False
   else (primeHelper n 2)

primeHelper :: Int -> Int -> Bool
primeHelper n d =
   if (n == d)
   then True
   else (( n `mod` d) /= 0) && (primeHelper n (d+1))



main :: IO()
main = do
    --Zadacha 1 test
     print (findSum 0 2 10)
     print (findSum 5 3 5)
     --Zadacha 2 test
     print (isSquare 1) -- True
     print (isSquare 2) -- False
     print (isSquare 4) -- True
     print (isSquare 17) -- False
     print (isSquare 256) -- True
     print (isSquare 2500) -- True
     --Zadacha 3 test
     print (isSpecial 131 2) -- True (числата 13 и 31 са прости)
     print (isSpecial 472 2) -- False (47 е просто число, но 72 не е просто)
     print (isSpecial 17197 2) -- True (числата 17, 71, 19 и 97 са прости)
     print (isSpecial 12234 3) -- False (числото 234 не е просто)
     print (isSpecial 10113 3) -- True (числата 101, 011 и 113 са прости)
     print (isSpecial 353 2) -- False (числото 35 не е просто)