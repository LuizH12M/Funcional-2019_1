data Nat = Zero | Succ Nat deriving Show 

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1)) 

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n) 


multiplies :: Nat -> Nat -> Nat
multiplies m Zero = Zero
multiplies m (Succ n) = add m (multiplies m n) 


--A binary tree is complete if the two sub-trees of
--every node are of equal size. Define a function
--that decides if a binary tree is complete. 

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)) 

t2 :: Tree Int
t2 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Node (Leaf 9) 2 (Leaf 8)))

--t3 :: Tree Int
--t3 = Node ((Leaf 1) 5 )


getSizeOfSubTreeLeft :: Tree a -> Int
getSizeOfSubTreeLeft (Leaf x) = 0
getSizeOfSubTreeLeft (Node l y r) = 1 + getSizeOfSubTreeLeft l

getSizeOfSubTreeRight :: Tree a -> Int
getSizeOfSubTreeRight (Leaf x) = 0
getSizeOfSubTreeRight (Node l y r) = 1 + getSizeOfSubTreeRight r

--getNumberOfSubTree :: Tree a -> Int
--getNumberOfSubTree (Leaf x) = 0
--getNumberOfSubTree (Node l y r) = 2

isComplete :: Tree a -> Bool
isComplete (Leaf x) = True
isComplete (Node l y r) = (getSizeOfSubTreeRight (Node l y r) == getSizeOfSubTreeLeft (Node l y r)) and isComplete l and isComplete r