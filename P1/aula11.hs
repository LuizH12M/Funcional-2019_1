--Exercicios pg. 89

-- 2.a

myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = False
myAll f (x:xs) = if length(filter f (x:xs)) == length(x:xs) then True else False

--myAll p xs = and (map p xs)

-- 2.b

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if length(filter f (x:xs)) == 0 then False else True

--myAny p xs = or (map p xs)

-- 2.c

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs) = if f x == True then x : myTakeWhile f xs else [] 

-- 2.d

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (x:xs) = if f x == True then myDropWhile f xs else (x:xs)

--Exercicio 3

-- map

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> (f x):xs) []

-- filter

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs) = foldr (\x xs -> if p x then x:xs else xs) [] (x:xs)

--dec2int :: [Int] -> Int
--dec2int (x:xs) = foldr (concat) [] (x:xs) 