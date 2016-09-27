module LHeapsTest exposing (..)

import Test exposing (..)
import Expect
import List exposing (sort)

import LHeaps exposing (..)

tests : Test
tests =
    describe "Homework 3" [
         describe "3.1.1" [
              test "foo" <| \() -> Expect.equal E (fromList []),
              test "foo" <| \() -> Expect.equal (T 1 1 E E) (fromList [1]),
              test "foo" <| \() -> Expect.equal
                  (T 2 1
                       (T 2 4
                          (T 2 5
                               (T 1 6 E E)
                               (T 1 9
                                    (T 1 10 E E)
                                    E))
                          (T 1 8 E E))
                       (T 1 3
                            (T 1 11 E E)
                            E))
                  (fromList [4, 8, 10, 9, 1, 3, 5, 6, 11])
                  ]
             ]
