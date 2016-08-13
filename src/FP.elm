module FP (..) where

import List

digit : Int -> Int -> List Int
digit n m =
    case n of
        0 -> []
        _ ->
          let s = n % m
              d = s // (m // 10)
          in
          d :: (digit (n - s) (m * 10))

digitsOfInt : Int -> List Int
digitsOfInt n =
    List.reverse <| digit n 10

addAll : List Int -> Int -> (Int, Int)
addAll l n =
  let s = List.sum l
  in
    if s < 10 then
      (s, n)
    else
      addAll (digitsOfInt s) (n + 1)


additivePersistence : Int -> Int
additivePersistence n =
    snd <| addAll (digitsOfInt n) 1


digitalRoot : Int -> Int
digitalRoot n =
    fst <| addAll (digitsOfInt n) 1


subsequences : List a -> List (List a)
subsequences xs =
    case xs of
      [] -> []
      [x] -> [[x],[]]
      x::ys ->
        let zs = subsequences ys
            addToAll = \x xs -> List.map ((::) x) xs
        in addToAll x zs ++ zs

take : Int -> List a -> Result String (List a)
take k xs =
  if k < 0 then
    Err "negative index"
  else if k > List.length xs then
    Err "not enough elements"
  else
    Ok <| List.take k xs
