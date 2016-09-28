module FPTest exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect
import List exposing (sort, reverse, length)
import FP exposing (..)
import String

isIntEqual i result =
  case result of
    Ok value -> value `Expect.equal` i
    Err err -> Expect.fail err

tests : Test
tests =
  describe "Homework 1" [
    describe "digitsOfInt with negative integers" [
      fuzz (intRange -1 -50) "return empty lists" <|
        \i -> digitsOfInt i `Expect.equal` []
    ]

  , describe "digitsOfInt with positive integers" [
      fuzz (intRange 0 (1*10^6)) "return non-empty lists" <|
        \i ->  1 `Expect.atLeast` length (digitsOfInt i)

    , fuzz (intRange 0 (1*10^8)) "joining the list returns the original number" <|
        \i ->
          i
          |> digitsOfInt
          |> List.map toString
          |> String.join ""
          |> String.toInt
          |> isIntEqual i
    ]

  , describe "1.1.2" [
      test "additivePersistance" <|
        \() -> Expect.equal 2 (additivePersistence 9876)

    , test "digitalRoot" <|
        \() -> Expect.equal 3 (digitalRoot 9876)
  ]

  , describe "1.1.3" [
      test "subsequences" <|
        \() ->
          Expect.equal
          [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
          (sort (subsequences [1..3]))
    ]

  , describe "1.1.4" [
      test "take 0" <|
        \() -> Expect.equal (Ok []) (take 0 [1..9])

    , test "take 5" <|
        \() -> Expect.equal (Ok [1,2,3,4,5]) (take 5 [1..9])

    , test "not enough elements" <|
        \() -> Expect.equal (Err ("not enough elements")) (take 10 [1..9])

    , test "negative index" <|
        \() -> Expect.equal (Err ("negative index")) (take -10 [1..9])
    ]
  ]
