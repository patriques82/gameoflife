module Main where

import Lib

main :: IO ()
main =
  run $ gamestate 5 [ 0, 0, 0, 0, 0
                    , 0, 1, 1, 1, 0
                    , 0, 0, 0, 0, 0
                    , 0, 0, 0, 0, 0
                    , 0, 0, 0, 0, 0
                    ]
