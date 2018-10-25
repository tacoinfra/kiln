module Main where

import Obelisk.Backend (runBackend)

import Backend (backendMain)

main :: IO ()
main = backendMain runBackend
