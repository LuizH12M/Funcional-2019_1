-- Exercicios pré prova

--List Comprehensions

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = [x | x <- (x:xs), even x]

primes :: [Int] -> [Int]
primes [] = []
primes (x:xs) = [x | x <- (x:xs), factors x == [1,x]]

-- Zip

parrs :: [Int] -> [Int] -> [(Int,Int)]
parrs xs ys = [(x,y) | (x,y) <- pares, even x, even y]
  where
    pares = zip xs ys

positions :: Eq a => a -> [a] -> [Int]
positions value [] = []
positions value xs = [i | (x', i) <- zip xs [0..], value == x']

somaDosQuadrados :: Int
somaDosQuadrados = sum [x * x | x <- [1..100]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [0..n], y <- [0..n], z <- [0..n], isPythagorean (x,y,z)]

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (x,y,z) = if x * x + y * y == z * z then True else False