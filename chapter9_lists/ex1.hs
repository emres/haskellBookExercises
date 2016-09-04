
splitAtChar :: Char -> String -> [String]
splitAtChar chr str
  | dropWhile (/= chr) str == "" = [str]
  | otherwise = [takeWhile (/= chr) str] ++
                splitAtChar chr (tail $ dropWhile (/= chr) str)


myWords :: [Char] -> [[Char]]
myWords str = splitAtChar ' ' str

-- takeWhile (/= ' ') "all i wanna do" ++
-- takeWhile (/= ' ') (tail $ dropWhile (/= ' ') "all i wanna do") ++
-- takeWhile (/= ' ') (tail $ dropWhile (/= ' ')
--                       ((tail $ dropWhile (/= ' ') "all i wanna do"))
-- takeWhile (/= ' ') (tail $ dropWhile (/= ' ')
--                     (tail $ dropWhile (/= ' ')
--                      ((tail $ dropWhile (/= ' ') "all i wanna do"))))   
