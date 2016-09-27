module ListsAndTreesTest where

import ElmTest exposing (..)
import List exposing (sort)

import ListsAndTrees exposing (..)

t = Node 5
    (Node 3
         (Node 1 Empty Empty)
         (Node 4 Empty Empty))
    (Node 7
         Empty
         (Node 9 Empty Empty))

size tree = case tree of
  Empty -> 0
  Node _ left right -> 1 + size left + size right

height tree = case tree of
  Empty -> 0
  Node _ left right -> 1 + max (height left) (height right)

tests : Test
tests =
    suite "Homework 2" [
        suite "2.1.1" [
            defaultTest <| assertEqual [[1,2,3,4],[2,3,4],[3,4],[4],[]]
                (suffixes [1..4])],
        suite "2.2.1" [
            defaultTest <| assertEqual True (mem 1 t),
            defaultTest <| assertEqual True (mem 3 t),
            defaultTest <| assertEqual True (mem 4 t),
            defaultTest <| assertEqual True (mem 5 t),
            defaultTest <| assertEqual True (mem 7 t),
            defaultTest <| assertEqual True (mem 9 t),
            defaultTest <| assertEqual False (mem 0 t),
            defaultTest <| assertEqual False (mem 2 t),
            defaultTest <| assertEqual False (mem 6 t),
            defaultTest <| assertEqual False (mem 8 t)],
        suite "2.2.2" [
            defaultTest <| assertEqual
                (Node 1 (Node 1 Empty Empty) (Node 1 Empty Empty))
                (fullTree 1 2),
            defaultTest <| assertEqual (Node 0 Empty Empty) (fullTree 0 1),
            defaultTest <| assertEqual Empty (fullTree 7 0),
            defaultTest <| assertEqual Empty (fullTree 5 -1)],
        suite "2.2.3" [
            defaultTest <| assertEqual Empty (balancedTree 1 0),
            defaultTest <| assertEqual (Node 1 Empty Empty) (balancedTree 1 1),
            defaultTest <| assertEqual 5 <| size (balancedTree 1 5),
            defaultTest <| assertEqual 3 <| height (balancedTree 1 5)
            ]
        ]
