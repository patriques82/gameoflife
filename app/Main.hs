module Main where

import Lib

main :: IO ()
main = run pentadecathlon

toad = grid 6 [ 0, 0, 0, 0, 0, 0
              , 0, 0, 0, 0, 0, 0
              , 0, 0, 1, 1, 1, 0
              , 0, 1, 1, 1, 0, 0
              , 0, 0, 0, 0, 0, 0
              , 0, 0, 0, 0, 0, 0
              ]

pulsar = grid 17 [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 ]

pentadecathlon = grid 11 [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                         ]
