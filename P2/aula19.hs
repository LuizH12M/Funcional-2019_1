--data Maybe a = Nothing | Just a

--instance Functor Maybe where
  --fmap _ Nothing = Nothing
  --fmap g (Just x) = Just (g x)

-- fmap id = id
-- fmap (g.h) = fmap g.fmap h

--Applicative Functor
--instance Applicative [] where
  --pure x = [x]
  --fs <*> xs = [f x | f <- fs, x <- xs]

  lista :: [Int] -> [Int] -> [Int]
  lista xs ys = [x * y | x <-  xs, y <- ys] 