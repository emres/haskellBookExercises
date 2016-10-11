import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

main :: IO ()
main = putStrLn "Let's do some Reader exercises!"
