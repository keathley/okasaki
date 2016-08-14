module FP where

import List exposing (..)

last : List Int -> Int
last = foldl (curry fst) 0

digitsOfInt : Int -> List Int
digitsOfInt n =
   if n < 0 then
      []
   else if n < 10 then
      [n]
   else
      digitsOfInt (n // 10) ++ [n % 10]

digitalRoots : Int -> List Int
digitalRoots n =
   if n < 10 then
      [n]
   else
      let s = digitsOfInt n |> sum
      in [s] ++ digitalRoots s

additivePersistence : Int -> Int
additivePersistence = digitalRoots >> length

digitalRoot : Int -> Int
digitalRoot = digitalRoots >> last

subsequences : List a -> List (List a)
subsequences xs =
   case xs of
   []      -> [[]]
   (x::xs) -> let ss = subsequences xs
              in ss ++ map ((::) x) ss

take : Int -> List a -> Result String (List a)
take k xs =
   if k < 0 then
      Err "negative index"
   else if k > length xs then
      Err "not enough elements"
   else
      Ok <| List.take k xs
