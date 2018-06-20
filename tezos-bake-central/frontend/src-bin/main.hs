module Main where

import Reflex.Dom

import Frontend

main :: IO ()
main = mainWidget $ snd frontend
