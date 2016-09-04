{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------
-- |1     |2 ABC |3 DEF |
-- ______________________
-- |4 GHI |5 JKL |6 MNO |
-- ----------------------
-- |7 PQRS|8 TUV |9 WXYZ|
-- ----------------------
-- |* ^   |0 +_  |# .,  |
-- ----------------------

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone =
  DaPhone [(Char, Digit, Presses)]
  deriving Show

myPhone :: DaPhone
myPhone =
  DaPhone [('a', '2', 1), ('b', '2', 2), ('c', '2', 3)]

getList :: DaPhone -> [(Char, Digit, Presses)]
getList (DaPhone list) = list

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone list) aChar =
  foldr f [] list
  where f (c, d, p)
          | c == aChar = ((d, p):)
        f _         = id       


myList :: [(Char, Char, Int)]
myList = [('a', '2', 1), ('b', '2', 2), ('c', '2', 3)]

myFunction :: [(Char, Char, Int)] -> Char -> [(Char, Int)]
myFunction aList aChar =
  foldr f [] aList
  where f (c, d, p)
          | c == aChar = ((d,p):)
        f _           = id
