module FPTest where

import ElmTest exposing (..)
import List exposing (sort)

import FP exposing (..)

tests : Test
tests =
    suite "Homework 1"
        [
          defaultTest <| assertEqual [4,1,2,4] (digitsOfInt 4124)
        , defaultTest <| assertEqual [0] (digitsOfInt 0)
        , defaultTest <| assertEqual 2 (additivePersistence 9876)
        , defaultTest <| assertEqual 3 (digitalRoot 9876)
        , defaultTest <| assertEqual [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
            (sort (subsequences [1..3]))
        , defaultTest <| assertEqual (Ok []) (take 0 [1..9])
        , defaultTest <| assertEqual (Ok [1,2,3,4,5]) (take 5 [1..9])
        , defaultTest <| assertEqual (Err ("not enough elements"))
            (take 10 [1..9])
        , defaultTest <| assertEqual (Err ("negative index"))
            (take -10 [1..9])
        ]
