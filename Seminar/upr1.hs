main :: IO()
main = do
    print 1
    print 2
    print 3
    print v1
    print (mymin 10 12)
    print (isinside 10 1 15)
    print (calcAverage 3 4)
    print (fibRec 5)
    print (fibIter 5)
    print (fibIter 500)

v1 :: Integer
v1 = 2 ^ 1024

mymin a b =
    if a < b then a else b

isinside x a b =
    a <= x && x <= b

calcAverage a b =
    (a * a + b * b) / 2

fibRec n =
    if n <= 1
    then 1
    else fibRec (n - 2) + fibRec (n - 1)

helper k prev cur =
    if k <= 1
    then cur
    else helper (k - 1) cur (prev + cur)

fibIter n =
    helper n 1 1