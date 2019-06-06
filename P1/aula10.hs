filter' p xs = [x | x <- xs, p x]

--foldr (função de alta ordem) foldr (+) 0 [1..10]
--all (todos)
--any (existe)
--takeWhile (enquanto for verdadeiro)
--dropWhile (ignorar o elementos enquanto a condição for verdadeira)
--map
--filter [f x | x <- xs, p x]

exercice2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exercice2 f p xs = map f (filter p xs) 

exercice2' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exercice2' f p = map f . filter p