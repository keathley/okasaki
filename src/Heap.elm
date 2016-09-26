module Heap exposing (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

import Array as A

type Heap = WrapHeap (A.Array Int)


pop : A.Array a -> A.Array a
pop a = A.slice 0 (A.length a - 1) a

empty : Heap
empty = WrapHeap A.empty

isEmpty : Heap -> Bool
isEmpty (WrapHeap a) = A.length a == 0

findMin : Heap -> Maybe Int
findMin (WrapHeap a) = A.get 0 a

insert : Int -> Heap -> Heap
insert x (WarpHeap a) =
    let
        n  = A.length a
        a' = A.push x a
    in
        WrapHeap (bubbleUp n a')
