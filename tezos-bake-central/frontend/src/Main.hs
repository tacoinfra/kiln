{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  text "Hello!"
