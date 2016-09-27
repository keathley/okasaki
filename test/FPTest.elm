module FPTest where

import ElmTest exposing (..)
import List exposing (sort)

import FP exposing (..)

tests : Test
tests =
    suite "Homework 1" [
        suite "1.1.1" [
            defaultTest <| assertEqual [4,1,2,4] (digitsOfInt 4124),
            defaultTest <| assertEqual [2,1,4,7,4,8,3,6,4,7]
                (digitsOfInt 2147483647),
            defaultTest <| assertEqual [2,1,4,7,4,8,3,6,4,7,0]
                (digitsOfInt 21474836470),
            defaultTest <| assertEqual [0] (digitsOfInt 0)],
        suite "1.1.2" [
            defaultTest <| assertEqual 2 (additivePersistence 9876),
            defaultTest <| assertEqual 3 (digitalRoot 9876)],
        suite "1.1.3" [
            defaultTest <| assertEqual
                [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
                (sort (subsequences [1..3]))],
       suite "1.1.4" [
            defaultTest <| assertEqual (Ok []) (take 0 [1..9]),
            defaultTest <| assertEqual (Ok [1,2,3,4,5]) (take 5 [1..9]),
            defaultTest <| assertEqual (Err ("not enough elements"))
                (take 10 [1..9]),
            defaultTest <| assertEqual (Err ("negative index"))
                (take -10 [1..9])
            ]
        ]
