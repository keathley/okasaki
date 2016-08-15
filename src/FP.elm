module FP where

import List
import String

digitsOfInt : Int -> List Int
digitsOfInt n =
  case n of
  n -> toString n |> String.split |> List.map String.toInt
  _ -> []

additivePersistence : Int -> Int
additivePersistence n =
  -- TODO
  0

digitalRoot : Int -> Int
digitalRoot n =
  -- TODO
  0

subsequences : List a -> List (List a)
subsequences xs =
  -- TODO
  []

take : Int -> List a -> Result String (List a)
take k xs =
  -- TODO
  Err "..."
