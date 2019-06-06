--Exercicio 1

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n], ((x^2)+(y^2)==(n^2))]

--Exercicio 2

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]

isPerfect :: Int -> Bool
isPerfect n
  | (sum [x | x <- [1..n-1], (mod n x == 0)]) == n = True
  | otherwise = False 

--Exercicio 3

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]   

