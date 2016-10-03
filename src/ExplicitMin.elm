module ExplicitMin
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

import BinomialHeaps as H
-- import LeftistHeaps as H

type Heap =
       E
     | NE Int H.Heap   -- the Int is the minimum element

empty : Heap
empty = E

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert _ _ =
  -- TODO
  E

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (E, h2') -> h2'
  (h1', E) -> h1'
  ((NE m1 h1'), (NE m2 h2')) ->
    if m1 <= m2 then
      NE m1 <| H.merge h1' h2'
    else
      NE m2 <| H.merge h1' h2'

findMin : Heap -> Maybe Int
findMin h = case h of
  E -> Nothing
  (NE m _) -> Just m

deleteMin : Heap -> Maybe Heap
deleteMin h = case h of
  E -> Nothing
  (NE m h) -> Just <| NE (H.findMin h) <| H.deleteMin h
