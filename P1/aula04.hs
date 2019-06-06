
--Exercicio B
prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * prod ns

--Exercice C
qsortDec :: Ord a => [a] -> [a]
qsortDec [] = []
qsortDec (x:xs) = qsortDec larger ++ [x] ++ qsortDec smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

--Exercice D - Elimina os Repetidos
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

--Exercice E

--Exercice F

--Exercice G
{-
((2^3)*4)
((2*3)+(4*5))
(2+(3*(4^5)))
-}

--Exercice H - Não, Entrada "[]"

--Exercice I - Nenhuma

--Exercice J
n::Int
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]