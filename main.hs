doubleNumber :: Integer -> Integer
doubleNumber x = x * 2

doubleNumbers :: [Integer] -> [Integer]
doubleNumbers [] = []
doubleNumbers (x:xs) = doubleNumber x :doubleNumbers xs

multiplyListByNumber :: Integer -> [Integer] -> [Integer]
multiplyListByNumber x = map ((*) x)

multiplyListByNumber2 :: Integer -> [Integer] -> [Integer]
multiplyListByNumber2 _ [] = []
multiplyListByNumber2 n (x:xs) = (n * x) : multiplyListByNumber2 n xs

sumList::[Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

factorial::Integer -> Integer
factorial 1 = 1 
factorial n = n * factorial (n-1)

primeList::[Integer] -> [Integer]
primeList [] = []
primeList (x:xs) = if (isPrime x) then x : primeList xs else primeList xs

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrimeRec n (n-1) 

isPrimeRec :: Integer -> Integer -> Bool
isPrimeRec _ 1 = True
isPrimeRec n m = if (n `mod` m) == 0 then False else isPrimeRec n (m-1)

nextPrime :: Integer -> Integer
nextPrime n = if (isPrime (n+1)) then (n+1) else nextPrime (n+1)

previusPrime :: Integer -> Integer
previusPrime 2 = 2
previusPrime n = if (isPrime (n-1)) then (n-1) else previusPrime (n-1)