myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : myInit xs

--Exercices Cap 6 Pg. 72

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1) 

--3
myPow :: Int -> Int -> Int
myPow _ 0 = 1
myPow x y = x * myPow x (y-1)

--4
euclid :: Int -> Int -> Int
euclid x 0 = 0
euclid 0 y = 0
euclid x y
    | x < y = if (mod y x) == 0 then x else euclid (mod y x) x 
    | x > y = if (mod x y) == 0 then y else euclid (mod x y) y
    | otherwise = x

--5


--6

--a
myAnd :: [Bool] -> Bool
myAnd [] = False
myAnd [x] = if x == True then True else False
myAnd (x:xs) = if x == True then myAnd xs else False

--d
(!!!) :: [a] -> Int-> a
(!!!) [] = error "Empty List"
(!!!) (x:xs) 0 = x 
(!!!) (x:xs) n = (!!!) xs (n-1)  

--9

--a 
sumOfAList :: [Int] -> Int
sumOfAList [] = 0
sumOfAList (x:xs) = x + sumOfAList xs

--b 
takeOfAList :: [a] -> Int -> [a]
takeOfAList [] _ = []
takeOfAList (x:xs) 1 = [x]
takeOfAList (x:xs) n = x : takeOfAList xs (n-1)

--c
lastElement :: [a] -> a
lastElement [] = error "Empty List"
lastElement [x] = x
lastElement (x:xs) = lastElement xs