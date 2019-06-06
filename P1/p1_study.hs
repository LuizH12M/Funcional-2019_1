vetor :: [Int]
vetor = [1,2,4,5,7,8]

binSearch :: Int -> [Int] -> Int
binSearch _ [] = -1
binSearch n (x:xs)
  | n == (x:xs) !! meio = 100
  | n > (x:xs) !! meio = binSearch n right
  | n < (x:xs) !! meio = binSearch n left
  where
    meio = div (length (x:xs)) 2
    left = take (meio-1) (x:xs) 
    right = drop (meio+1) (x:xs)


insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) 
  | n > x = x : insert n xs
  | n < x = n : xs
  
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- TREES

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)) 

t2 :: Tree Int
t2 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Node (Leaf 9) 2 (Leaf 8)))


getSizeOfSubTreeLeft :: Tree a -> Int
getSizeOfSubTreeLeft (Leaf x) = 0
getSizeOfSubTreeLeft (Node l y r) = 1 + getSizeOfSubTreeLeft l

getSizeOfSubTreeRight :: Tree a -> Int
getSizeOfSubTreeRight (Leaf x) = 0
getSizeOfSubTreeRight (Node l y r) = 1 + getSizeOfSubTreeRight r

isComplete :: Tree a -> Bool
isComplete (Leaf x) = True
isComplete (Node l y r) = (getSizeOfSubTreeRight (Node l y r) == getSizeOfSubTreeLeft (Node l y r)) and isComplete l and isComple