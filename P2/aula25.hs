{-
EXERCÍCIO 01: 
    Por que a avaliação mais externa (outmost) de fst
    (1+2, 2+3) é preferível à avaliação mais interna
    (innermost)?

    With (leftmost) innermost evaluation:
    fst ((1+2), (2+3))
    = fst (3, (2+3))
    = fst (3, 5)
    = 3
    With (leftmost) outermost evaluation:
    1
    fst ((1+2), (2+3))
    = (1+2)
    = 3

    Ou seja, a outmost é preferível pelo número de operações.
    -}

{-
EXERCÍCIO 02:
    Um número Armstrong é um número cuja soma de seus dígitos elevados à quantidade de números do dígito 
    resulta no mesmo. Por exemplo:
    ■ 9 é um número Armstrong porque 9^1 = 9
    ■ 10 não é um número Armstrong porque 1^2 + 0^2 /= 10
    ■ 153 é um número Armstrong porque 1^3 + 5^3 + 3^3 = 153
    ■ 154 não é um número Armstrong porque 1^3 + 5^3 + 4^3 /= 154
    Crie uma função que determina se um número é um número
    Armstrong ou não.
-}

digitos :: Int -> [Int]
digitos 0 = []
digitos n = digitos (div n 10)
 ++ [mod n 10]

armstrong :: Int -> Bool
armstrong n = sum[x ^ length (digitos n) | x <- (digitos n)] == n
           -- where exp <- digitos n

{-
EXERCÍCIO 03:
    Usando list comprehension, defina uma expressão fibs:: [Integer] que gera uma sequência infinita 
    de números de Fibonacci
        1, 1, 2, 3, 5, 8, 13, 21, 34, . . .
    usando o seguinte procedimento:
        a. Os primeiros dois números são 0 e 1;
        b. O próximo número é a soma dos dois anteriores.
        c. Repita o passo anterior.
    Dica: use as funções padrão zip e tail como auxiliares. O
    tipo Integer permite o uso de inteiros com precisão arbitrária.
-}
-- f (x, y) = x + y
-- n = [(1,2), (3, 4)]
-- map f n
-- [3, 7]
 
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip (fibs) (tail (fibs))] 

-- Utilizando funções: (mas não pode)
-- fib :: Int -> Int -> [Int]
-- fib a b = a : fib b (a + b)