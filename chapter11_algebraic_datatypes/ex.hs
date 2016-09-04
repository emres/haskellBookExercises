{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
   tooMany n = n > 42

-- instance TooMany (Int, String) where
--   tooMany (n, s) = n > 42

-- newtype Goats = Goats Int deriving Show
-- data Goats = Goats Int deriving Show

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

data Person =
  Person { name :: String
         , age :: Int}
  deriving (Eq, Show)

data OperatingSystem =
       GnuPlusLinux
       | OpenBSDPlusNevermindJustBSDStill
       | Mac
       | Windows
       deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]
  
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = map
  (\(o,l) -> Programmer {os = o, lang = l})
  [(o,l) | o <- allOperatingSystems, l <- allLanguages]



data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- filling in some details to help you along
-- Note, you do *not* need to use insert' for this.
-- Retain the original structure of the tree.

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


preorder :: BinaryTree a -> [a]
preorder Leaf  = []
preorder (Node left a right) =
  [a] ++ preorder left ++ preorder right

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

