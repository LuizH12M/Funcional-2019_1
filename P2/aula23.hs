-- > 1. Implementar a função 'Choices' sem usar map, composição e concat. 
--   Use list comprehension. Dica: Use perms e subs
--   Funções necessárias
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
--
-- 'Choices' normal:
choices :: [a] -> [[a]]
choices = concat . map perms . subs
-- 'Choices' com List Comprehension
choicesList :: [a] -> [[a]]
choicesList xs = [zx | ys <- subs xs, zx <- perms ys]

-- > 2. Defina uma função recursiva 'isChoice :: Eq a => [a] -> [a] -> Bool
--      que decide se uma lista s está em choices U. Tanto s quanto u são
--      listas do mesmo tipo. Não use 'subs' nem 'perms.

isChoice :: Eq a => [a] -> [a] -> Bool
--isChoice [] [] = True
isChoice xs ys = if xs == aux then True else isChoice xs (tail ys)
    where aux = head (choices(ys))

