module FP where

import List   -- TODO: modify imports if you'd like
-- import Arithmatic

digitsOfInt : Int -> List Int
digitsOfInt n = case n of
                n == 0 -> n
                _ -> let t = n % 10
                           h = n // 10
                           in
                               (digitsOfInt h)::t
  -- TODO
  -- []

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
