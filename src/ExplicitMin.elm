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
insert x h = case h of
  E -> NE x <| H.insert x H.empty
  NE x' h' -> NE (min x x') <| H.insert x h'

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2
  (NE x1 h1', NE x2 h2') -> NE (min x1 x2) <| H.merge h1' h2'

findMin : Heap -> Maybe Int
findMin h = case h of
  E -> Nothing
  NE x _ -> Just x

deleteMin : Heap -> Maybe Heap
deleteMin h = case h of
  E -> Nothing
  NE x h' -> let h'' = Maybe.withDefault H.empty <| H.deleteMin h' in
                case H.findMin h'' of
                    Nothing -> Nothing
                    Just x' -> Just <| NE x' h''

  -- Alternate implementation (without withoutDefault)

  -- NE x h' ->
  --     case H.deleteMin h' of
  --         Nothing -> Nothing
  --         Just h'' -> case H.findMin h'' of
  --             Nothing -> Nothing
  --             Just x' -> Just <| NE x' h''
