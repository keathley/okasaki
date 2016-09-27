module ListsAndTreesTest exposing (..)

import Test exposing (..)
import Expect
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
    describe "Homework 2" [
        describe "2.1.1" [
            test "foo" <| \() -> Expect.equal [[1,2,3,4],[2,3,4],[3,4],[4],[]]
                (suffixes [1..4])],
        describe "2.2.1" [
            test "foo" <| \() -> Expect.equal True (mem 1 t),
            test "foo" <| \() -> Expect.equal True (mem 3 t),
            test "foo" <| \() -> Expect.equal True (mem 4 t),
            test "foo" <| \() -> Expect.equal True (mem 5 t),
            test "foo" <| \() -> Expect.equal True (mem 7 t),
            test "foo" <| \() -> Expect.equal True (mem 9 t),
            test "foo" <| \() -> Expect.equal False (mem 0 t),
            test "foo" <| \() -> Expect.equal False (mem 2 t),
            test "foo" <| \() -> Expect.equal False (mem 6 t),
            test "foo" <| \() -> Expect.equal False (mem 8 t)],
        describe "2.2.2" [
            test "foo" <| \() -> Expect.equal
                (Node 1 (Node 1 Empty Empty) (Node 1 Empty Empty))
                (fullTree 1 2),
            test "foo" <| \() -> Expect.equal (Node 0 Empty Empty) (fullTree 0 1),
            test "foo" <| \() -> Expect.equal Empty (fullTree 7 0),
            test "foo" <| \() -> Expect.equal Empty (fullTree 5 -1)] --,
        -- describe "2.2.3" [
        --     test "foo" <| \() -> Expect.equal Empty (balancedTree 1 0),
        --     test "foo" <| \() -> Expect.equal (Node 1 Empty Empty) (balancedTree 1 1),
        --     test "foo" <| \() -> Expect.equal 5 <| size (balancedTree 1 5),
        --     test "foo" <| \() -> Expect.equal 3 <| height (balancedTree 1 5)
        --     ]
        ]
