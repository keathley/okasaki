module FPTest exposing (..)

import Test exposing (..)
import Expect
import List exposing (sort)
import FP exposing (..)

tests : Test
tests =
    describe "Homework 1" [
        describe "1.1.1" [
            test "digitsOfInt" <| \() -> Expect.equal [4,1,2,4] (digitsOfInt 4124),
            test "digitsOfInt" <| \() -> Expect.equal [2,1,4,7,4,8,3,6,4,7]
                (digitsOfInt 2147483647),
            test "digitsOfInt" <| \() -> Expect.equal [2,1,4,7,4,8,3,6,4,7,0]
                (digitsOfInt 21474836470),
            test "digitsOfInt" <| \() -> Expect.equal [0] (digitsOfInt 0)],
        describe "1.1.2" [
            test "foo" <| \() -> Expect.equal 2 (additivePersistence 9876),
            test "foo" <| \() -> Expect.equal 3 (digitalRoot 9876)],
        describe "1.1.3" [
            test "foo" <| \() -> Expect.equal
                [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
                (sort (subsequences [1..3]))],
       describe "1.1.4" [
            test "foo" <| \() -> Expect.equal (Ok []) (take 0 [1..9]),
            test "foo" <| \() -> Expect.equal (Ok [1,2,3,4,5]) (take 5 [1..9]),
            test "foo" <| \() -> Expect.equal (Err ("not enough elements"))
                (take 10 [1..9]),
            test "foo" <| \() -> Expect.equal (Err ("negative index"))
                (take -10 [1..9])
            ]
        ]
