module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it?
-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- But we can assert a more specific type for liftedReplace!

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- The [] around Char is the 𝑓 of f Char, or the structure we li ed over. The 𝑓
-- of f a is the outermost [] in [Maybe [Char]]. So, 𝑓 is instantiated to []
-- when we make the type more specific, whether by applying it to a value of
-- type [Maybe [Char]] or by means of explicitly writing liftedReplace'.

-- What if we lift it twice?
-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f ~ []
-- f1 ~ Maybe


-- What if we lifted it thrice?
-- Prelude> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
-- :: (Functor f2, Functor f1, Functor f) =>
--    f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []

------------- Exercises: Heavy Lifting -------------------
a = (+1) <$> (read "[1]" :: [Int])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = (*2) <$> (\x -> x - 2)
d = ( (return '1' ++) . show ) <$> (\x -> [x, 1..3])

e =
  let ioi = (read "1" :: Integer)
      changed = read ( ("123" ++) <$> show $ ioi )
  in (*3) changed
----------------------------------------------------------

main :: IO ()
main = do
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)

  putStr "liftedReplace lms:   "
  print (liftedReplace lms)

  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:     "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:    "
  print (twiceLifted' lms)

  putStr "thriceLifted lms:    "
  print (thriceLifted lms)

  putStr "thriceLifted' lms:   "
  print (thriceLifted' lms)
