halve:: [a] -> ([a],[a])
halve [] = ([],[])
halve (x:xs) = (metade1, metade2)
  where
  metade = (div (length (x:xs)) 2)
  metade1 = take metade (x:xs)
  metade2 = drop metade (x:xs)

third1 :: [a] -> a
third1 (x:xs) = head(tail(tail xs))

third2 :: [a] -> a
third2 (x:xs) = (x:xs) !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

luhn:: [Integer] -> Bool
luhn [] = False
luhn (x:xs) = valida
  where
  etapa1 = dobraAlternado (x:xs)
  etapa2 = subtrai9 etapa1
  somaDeElementos = sum etapa2
  valida = if (mod somaDeElementos 10) == 0 then True else False 

dobraAlternado :: [Integer] -> [Integer]
dobraAlternado [] = []
dobraAlternado [x] = [x]
dobraAlternado (x:y:xs) = x : dobraNumero y : dobraAlternado xs

dobraNumero :: Integer -> Integer
dobraNumero x = x * 2

subtrai9::[Integer] -> [Integer]
subtrai9 [] = []
subtrai9 (x:xs) = (if x <= 9 then x else (x - 9)) : subtrai9 xs

intToList:: Integer -> Int
intToList n