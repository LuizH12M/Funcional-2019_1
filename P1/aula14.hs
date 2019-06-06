import System.IO 

act :: IO (Char,Char)
act = 
    do x <- getChar
       getChar
       y <- getChar
       return (x,y)

getLinha :: IO String
getLinha = do x <- getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLinha
                    return (x:xs) 

putStr2 :: String -> IO ()
putStr2 [] = return ()
putStr2 (x:xs) = do putChar x
                    putStr2 xs

putStrLn2 :: String -> IO ()
putStrLn2 xs = do putStr2 xs
                  putChar '\n' 

strlen :: IO ()
strlen = do putStr2 "Enter a string: "
            xs <- getLinha
            putStr2 "The string has "
            putStr2 (show (length xs))
            putStrLn2 " characters" 



hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word 

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs) 

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x 

play :: String -> IO ()
play word = 
   do putStr "? "
      guess <- getLine
      if guess == word then
         putStrLn "You got it!"
      else
         do putStrLn (match word guess)
            play word 

match :: String -> String -> String
match xs ys = 
   [if elem x ys then x else '-' | x <- xs]

     
   
   