module Main where

import Nagios.Config.EDSL
import Nagios.Config.EDSL.Defaults (linuxServer)

import Example.Localhost

main :: IO ()
main = putStr $ writeConfiguration toplevel

toplevel :: [Object]
toplevel =
    [ OHost localhost
    ]
