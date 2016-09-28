module LHeapsTest exposing (..)

import Test exposing (..)
import Fuzz exposing (list, int)
import Expect
import List exposing (sort, length)

import LHeaps exposing (..)

validateHeap : (Heap -> Bool) -> Heap -> List Bool
validateHeap assertion h =
  case h of
    E -> [True]
    _ ->
      [assertion h] ++
      validateHeap assertion (right h) ++
      validateHeap assertion (left h)

tests : Test
tests =
  describe "Leftist Heaps" [
    fuzz (list int) "size is equal to the size of the list" <|
      \genList ->
        size (fromList genList) `Expect.equal` length genList

  , fuzz (list int) "ranks on the left are greater then ranks on the right" <|
      \genList ->
        let
          heap = fromList genList

          assertRank h = rank (left h) >= rank (right h)
        in
          Expect.true
            "Expected all of the left ranks to be less than the right ranks"
            (List.all identity (validateHeap assertRank heap))

  , fuzz (list int) "values of the heap are always less than children values" <|
      \genList ->
        let
          heap = fromList genList
          assertValue h =
            value h <= value (left h) && value h <= value (right h)
        in
          Expect.true
            "Expected the children values to be greater then the parent"
            (List.all identity (validateHeap assertValue heap))


  , describe "3.1.1" [
      test "empty lists" <|
        \() -> Expect.equal E (fromList [])

    , test "lists with 1 element" <|
        \() -> Expect.equal (T 1 1 E E) (fromList [1])

    , test "list" <|
        \() -> Expect.equal
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
