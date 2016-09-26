module LHeaps where

import LeftistHeaps exposing (..)
import List exposing (..)

singleton : Int -> Heap
singleton x = insert x empty

fromList : List Int -> Heap
fromList l =
   case map singleton l |> makePass of
      []   -> empty
      h::_ -> h

mergePairs : List Heap -> List Heap
mergePairs l =
   case l of
      x::[]    -> l
      x::y::xs -> merge x y :: mergePairs xs
      _        -> []

makePass : List Heap -> List Heap
makePass l =
   case l of
      []    -> []
      h::[] -> [h]
      _     -> mergePairs l |> makePass
