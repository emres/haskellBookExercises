-- mTh x y z = x * y * z
-- mTh x y = \z -> x * y * z
-- mTh x = \y -> \z -> x * y * z
-- mTh = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> (\x -> x + 1) n
  False -> n
  
-- addFive x y = (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x

g :: (a -> b) -> (a, c) -> (b, c)
-- g f (a,c) = (f(a), c)
g f (a, c) = (f a, c)

-- g f (a, c) = case f(a) of
--   (a, c) -> (f(a), c)

roundTrip :: (Show a, Read a) => a -> a
--roundTrip a = read (show a)
roundTrip = read . show

main = do
  print (roundTrip 4)
  print (id 4)
