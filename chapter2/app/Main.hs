module Main where

import Lib
import Learn
  
main :: IO ()
main = someFunc

sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3
