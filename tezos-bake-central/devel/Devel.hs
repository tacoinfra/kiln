module Devel where

import Obelisk.Run

import Backend
import Frontend

main :: Int -> IO ()
main port = backendMain (run port)--  (backend mempty) frontend

