module ListsAndTreesTest where

import ElmTest exposing (..)
import List exposing (sort)

import ListsAndTrees exposing (..)

tests : Test
tests =
    suite "Homework 2" [
        suite "2.1.1" [
            defaultTest <| assertEqual [[1,2,3,4],[2,3,4],[3,4],[4],[]]
                (suffixes [1..4])],
        suite "2.2.1" [defaultTest <| assertEqual "TODO" "Add tests"],
        suite "2.2.2" [defaultTest <| assertEqual "TODO" "Add tests"],
        suite "2.2.3" [defaultTest <| assertEqual "TODO" "Add tests"]
        ]
