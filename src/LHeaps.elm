module LHeaps where

{-- Copied from LeftistHeaps.elm from class. DO NOT MODIFY. -------------}

type alias Rank = Int

type Heap = E | T Rank Int Heap Heap

rank : Heap -> Rank
rank h =
  case h of
    E         -> 0
    T r _ _ _ -> r

makeT : Int -> Heap -> Heap -> Heap
makeT x h1 h2 =
  let (left,right) =
    if rank h1 >= rank h2
      then (h1, h2)
      else (h2, h1)
  in
  T (1 + rank right) x left right

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2
  (T _ x1 left1 right1, T _ x2 left2 right2) ->
    if x1 <= x2
      then makeT x1 left1 (merge right1 h2)
      else makeT x2 left2 (merge h1 right2)

{------------------------------------------------------------------------}

singleton : Int -> Heap
singleton x = T 1 x E E

-- I'm not sure how to form the recurrence relations mentioned in the
-- assignment; what I do know is:
-- . mergePairs and makePass both run in O(n/2) time (since they both merge
--   elements two at a time), which reduces to O(n).

fromList : List Int -> Heap
fromList is = makePass <| mergePairs is

mergePairs : List Int -> List Heap
mergePairs is = case is of
  [] -> []
  [i] -> [singleton i]
  (i1::i2::is) -> (merge (singleton i1) (singleton i2)) :: mergePairs is

makePass : List Heap -> Heap
makePass hs = case hs of
  [] -> E
  [h] -> h
  [h1,h2] -> merge h1 h2
  (h1::h2::hs) -> merge (merge h1 h2) (makePass hs)
