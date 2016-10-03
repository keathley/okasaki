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
isEmpty = (==) E

insert : Int -> Heap -> Heap
insert i h =
   case h of
      E        -> NE i <| H.insert i H.empty
      NE j h   -> if i < j then
                     NE i <| H.insert i h
                  else
                     NE j <| H.insert i h

merge : Heap -> Heap -> Heap
merge h1 h2 =
   case (h1, h2) of
      (_, E)               -> h1
      (E, _)               -> h2
      (NE i h1, NE j h2)   -> if i < j then
                                 NE i <| H.merge h1 h2
                              else
                                 NE j <| H.merge h1 h2

findMin : Heap -> Maybe Int
findMin h =
   case h of
      E      -> Nothing
      NE i h -> Just i

deleteMin : Heap -> Maybe Heap
deleteMin h =
   case h of
      E        -> Nothing
      NE _ h   -> case H.deleteMin h of
                     Nothing ->  Nothing
                     Just h  ->  case H.findMin h of
                                    Nothing -> Nothing
                                    Just m  -> Just <| NE m h
