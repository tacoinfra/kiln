{-# LANGUAGE BangPatterns #-}

import Backend
import Obelisk.Backend

main :: IO ()
main = backendMain runBackend
