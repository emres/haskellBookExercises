myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (myOr . map f)

myElem :: Eq a => a -> [a] -> Bool
myElem = (myAny . (==))

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f)  []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr g []
  where g x =
          if f(x)
          then (x:)
          else id

--squish :: [[a]] -> [a]
squish xss = foldr (++) [] xss
--  where f [[]] = []
--        f [(x:xs)] = x : f(xs)
