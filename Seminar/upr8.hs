import Data.Char
import Data.List

main :: IO()
main = do
    print (ord 'a')
    print (chr 50)
    print (fromIntegral 2)
    print (ceiling (2.0 ^ 10))
    print (ceiling 2.56)
    print (floor 2.56)
    print (floor 2.0)
    print (2345678 `div` (ceiling (2.0 ^ 10)))
    print (normalize "Attack London tomorrow at ten a.m.")
    --print (normalize "Attack London tomorrow at 10 a.m.")
    print (encode ['A'..'Z'] 'A' 1)    -- = 'B'
    print (encode ['A'..'Z'] 'C' 2)    -- = 'E'
    print (encode ['A'..'Z'] 'Z' 3)    -- = 'C'
    print (encode ['A'..'Z'] 'A' (-1)) -- = 'Z'
    print (encode ['A'..'Z'] 'C' (-2)) -- = 'A'
    print (encode ['A'..'Z'] 'Z' (-3)) -- = 'W'
    --print (encode ['A'..'Z'] '@' 1)    -- = error “unsupported symbol: @”
    print (((encrypt ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (((encrypt1 ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (((encrypt2 ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (((decrypt ['A'..'Z'] 5) . (encrypt ['A'..'Z'] 5) . normalize) "Attack London tomorrow at ten a.m.")
    print (getTotal shop1)
    print (buy "bread" 2 shop1)
    print (buy "bread" 3 shop1)
    print (buy "bread" 4 shop1)
    --print (buy "bread1" 4 shop1)
    print (getNeeded shop1 1)
    print (getAverage shop1)
    print (getAverage1 shop1)
    print (closestToAverage shop1)
    print (cheaperAlternative shop2)

-- String == [Char]
normalize :: String -> String
normalize "" = ""
normalize (c:cs)
    | '0' <= c && c <= '9' = error "digits not allowed"
    | 'A' <= c && c <= 'Z' = c:normalize cs
    | 'a' <= c && c <= 'z' = chr (ord c - ord 'a' + ord 'A'):normalize cs
    | otherwise            = normalize cs

encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset =
    if offset >= 0
    then offsetSymbol (findSymbol alphabet ch) offset
    else encode (reverse alphabet) ch (-offset)
    where
        findSymbol [] _ = error "unsupported symbol: @"
        findSymbol alphabet@(a:as) ch =
            if a == ch then alphabet else findSymbol as ch
        
        offsetSymbol []     offset = offsetSymbol alphabet offset
        offsetSymbol (a:_)  0      = a
        offsetSymbol (_:as) offset = offsetSymbol as (offset - 1)

encrypt :: [Char] -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet ch offset | ch <- normalized]

encrypt1 :: [Char] -> Int -> String -> String
encrypt1 alphabet offset normalized = map (\ ch -> encode alphabet ch offset) normalized

encrypt2 :: [Char] -> Int -> String -> String
encrypt2 alphabet offset = map (\ ch -> encode alphabet ch offset)

decrypt :: [Char] -> Int -> String -> String
decrypt alphabet offset = encrypt alphabet (-offset)

-- https://learn.fmi.uni-sofia.bg/pluginfile.php/253060/mod_resource/content/1/ex8.pdf
type Product = (String, Int, Double)
type Shop = [Product]

shop1 :: Shop
shop1 = [("bread", 3, 1), ("milk", 1, 2.5), ("lamb", 1, 10),
         ("cheese", 1, 5), ("butter", 2, 2.3)]

shop2 :: Shop         
shop2 = [("bread", 1, 1), ("cheese", 1, 2.5), ("bread", 1, 1),
         ("cheese", 1, 5), ("butter", 1, 2.3)]


getPrice :: Product -> Double
getPrice (_, _, price) = price

getTotal :: Shop -> Double
getTotal shop = sum [(fromIntegral qty) * price | (_, qty, price) <- shop]

buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "Not found!"
buy name qty (p@(name1, qty1, price1):ss)
    | name == name1 && qty < qty1  = (name1, qty1 - qty, price1):ss
    | name == name1 && qty == qty1 = ss
    | name == name1 && qty > qty1  = p:ss
    | otherwise                    = buy name qty ss

getNeeded :: Shop -> Int -> [Product]
getNeeded shop minQty = [p | p@(_, qty, _) <- shop, qty <= minQty]

average :: [Double] -> Double
average ps = sum ps / (fromIntegral (length ps))

getAverage :: Shop -> Double
getAverage shop = average (map getPrice shop)

getAverage1 :: Shop -> Double
getAverage1 = average . (map getPrice)

getName :: Product -> String
getName (name, _, _) = name

closestToAverage :: Shop -> String
closestToAverage shop =
    getName (foldl1 (\ p1@(_,_,pr1) p2@(_,_,pr2) 
                     -> if abs (averagePrice - pr1) < abs (averagePrice - pr2)
                        then p1 else p2) shop)
    where averagePrice = getAverage shop

cheaperAlternative :: Shop -> Int
cheaperAlternative shop = length (filter moreThanTwoPrices (groupPrices shop))
    where
        products = nub (map getName shop)
        groupPrices shop = [[price | (name, _, price) <- shop, name == name1] | name1 <- products]
        moreThanTwoPrices prices = length (nub prices) > 1