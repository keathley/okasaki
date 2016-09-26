module LeftistHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

import Random exposing (maxInt)

type alias Rank = Int

type Heap = E | T Rank Int Heap Heap

rank : Heap -> Rank
rank h =
  case h of
    E         -> 0
    T r _ _ _ -> r

-- NOTE: different shapes if checking r1 > r2 instead
-- makeT x h1 h2 =
--   let (r1,r2) = (rank h1, rank h2) in
--   if r1 >= r2
--     then T (1+r2) x h1 h2
--     else T (1+r1) x h2 h1

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

insert : Int -> Heap -> Heap
insert x h = merge (singleton x) h

deleteMin : Heap -> Maybe Heap
deleteMin h =
  case h of
    E         -> Nothing
    T r _ a b -> Just (merge a b)

findMin : Heap -> Maybe Int
findMin h =
  case h of
    E         -> Nothing
    T _ i _ _ -> Just i

isEmpty : Heap -> Bool
isEmpty h = h == empty

empty       = E
singleton x = T 1 x E E

------------------------------------------------------------------------------

{-

computeRank : Heap -> Rank
computeRank h =
  case h of
    E -> 0
    T r _ left right ->
      let r' = 1 + computeRank right in
      if r == r'
        then r
        else Debug.crash "incorrect rank"

size : Heap -> Int
size h =
   case h of
     E         -> 0
     T _ _ a b -> 1 + size a + size b

value : Heap -> Int
value h =
  case h of
    E         -> maxInt
    T _ i _ _ -> i

left : Heap -> Heap
left h =
  case h of
    E         -> E
    T _ _ a _ -> a

right : Heap -> Heap
right h =
  case h of
    E         -> E
    T _ _ _ b -> b

log = logBase 2

-}
