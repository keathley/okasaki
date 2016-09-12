module FP where

import List exposing (sum, length, foldl, map)

digitsOfInt : Int -> List Int
digitsOfInt n = if n < 10 then [n] else digitsOfInt (n // 10) ++ [n % 10]

digitSums : Int -> List Int
digitSums n = if n < 10 then [n] else n :: (digitsOfInt >> sum >> digitSums) n

additivePersistence : Int -> Int
additivePersistence = digitSums >> length >> (+) -1

last = foldl (\x _ -> x) 0

digitalRoot : Int -> Int
digitalRoot = digitSums >> last

subsequences : List a -> List (List a)
subsequences xs = case xs of
  x::xs -> let xsSubs = subsequences xs in
             map ((::) x) xsSubs ++ xsSubs
  [] -> [[]]

take : Int -> List a -> Result String (List a)
take k xs =
  if k < 0 then
    Err "negative index"
  else if k > length xs then
    Err "not enough elements"
  else
    Ok (List.take k xs)
