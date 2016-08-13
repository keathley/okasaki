module FP where

import List

digitsOfIntRev : Int -> List Int
digitsOfIntRev n = case n of
    0 -> []
    _ -> (n % 10) :: (digitsOfIntRev (n // 10))

digitsOfInt : Int -> List Int
digitsOfInt n = List.reverse(digitsOfIntRev n)

ap n m =
    if n > 9 then
        ap (n |> digitsOfIntRev |> List.sum) (m + 1)
    else
        m

additivePersistence : Int -> Int
additivePersistence n = ap n 0

digitalRoot : Int -> Int
digitalRoot n =
    if n > 9 then
        n |> digitsOfIntRev |> List.sum |> digitalRoot
    else
        n

subsequences : List a -> List (List a)
subsequences xs =
    case xs of
        [] -> [[]]
        x::xs -> subsequences xs ++ List.map (\xs -> x::xs) (subsequences xs)

take : Int -> List a -> Result String (List a)
-- take k xs =
--     if k <  0 then
--         Err "negative index"
--     else if k == 0 then
--         Ok []
--     else case xs of
--              [] -> Err "too long"
--              x::xs -> take (k - 1) xs `Result.andThen` Ok (curry :: x)

take k xs =
    if k < 0 then Err "negative index"
    else if k > (List.length xs) then Err "not long enough"
    else Ok (List.take k xs)
