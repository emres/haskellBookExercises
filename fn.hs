-- myFunctionWith1Argument :: Integer -> Integer
-- myFunctionWith1Argument x = x * 99


-- f1 :: b -> c
-- f1 1 = 2

-- f2 :: a -> b
-- f2 3 = 1

-- f3 :: a -> c
-- f3 3 = 2

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g a = f (g a) 


a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f a = f a
