{-
Exercício 01:
    Defina uma função que calcula todas as quádruplas diferentes (a, b, c, d) dentro
    do intervalo 0 < a, b, c, d 6 n tal que a3 + b3 = c3 + d3.
    Exemplo 13 + 123 = 93 + 103 = 1729
    O problema original define a sequência Taxicab number
-}

-- taxiCab :: Int -> [(Int, Int, Int, Int)]
-- taxiCab n = [(a, b, c, d) | a <- [1..n], b <- [a..n], c <- [(a+1)..n], d <- [c..n], 
--             a^3 + b^3 == c^3 + d^3]

-- OU:

taxiCab :: Int -> [(Int, Int, Int, Int)]
taxiCab n = [(a, b, c, d) | a <- [1..n], b <- [a..n], c <- [(a+1)..n], d <- [c..n], 
            (cubo a + cubo b) == (cubo c + cubo d)]
            where cubo n = n * n * n
------------------------------------------------------------------------------------
{-
Exercício 02:  
    Defina uma função: disjoint :: (Ord a) => [a] -> [a] -> Bool
    que recebe duas listas em ordem ascendente e 
    determina se ambas possuem ou não algum elemento em comum.
-}

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = not (or [x==y | x<- xs, y <- ys])

-- OU:

disjoint' :: (Ord a) => [a] -> [a] -> Bool
disjoint' xs [] = True
disjoint' [] ys = True
disjoint' (x:xs) (y:ys) | x < y = disjoint' xs (y:ys)
                        | x == y = False
                        | x > y = disjoint' (x:xs) ys
-- casamento de padrao

{-
Exercício 03:
Defina uma função
    balance :: [a] -> Tree a
que converte uma lista não-vazia em uma árvore
balanceada.
Dica: primeiro defina uma função que divide uma lista em duas partes cujo tamanho de uma lista difere 
em no máximo 1 em relação ao tamanho da outra.
-}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tBal :: Tree Int
tBal = Node  (Node (Leaf 1) (Leaf 4)) 
                (Node (Leaf 6) (Leaf 9)) 

balance     :: [a] -> Tree a
balance []  =  error "lista vazia!"
balance [x] =  Leaf x
balance xs  =  Node (balance (fst (halveList xs))) (balance (snd (halveList xs)))  

halveList :: [a] -> ([a],[a])
halveList [] = ([],[])
halveList xs = splitAt (half xs) xs
             where
                  half :: [a] -> Int
                  half xs = div (length xs) 2 