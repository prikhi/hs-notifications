module Main where

import Data.Default (def)

import HsNotifications (run)

main :: IO ()
main =
    run def
