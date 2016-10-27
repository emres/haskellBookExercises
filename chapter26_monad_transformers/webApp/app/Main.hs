{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Web.Scotty

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    --lift $ putStrLn $ "hello"
    liftIO $ putStrLn $ "hello" 
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

