module BHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

import List as L

type Tree = Node Int (List Tree)
type alias Rank = Int
type alias InternalHeap = List (Rank, Tree)
type Heap = WrapHeap InternalHeap

{-- Internal Helpers ----------------------------------------------------}
root : Tree -> Int
root (Node x cs) = x

rank : Tree -> Int
rank (Node _ ts) = case ts of
  [] -> 0
  ts -> let height = floor <| logBase 2 <| toFloat <| L.length ts
            full = 2 ^ height
        in
            if L.length ts == full then
              height + 1
            else
              height

link : Tree -> Tree -> Tree
link t1 t2 =
  let (Node x1 c1) = t1
      (Node x2 c2) = t2
  in
    if x1 <= x2
      then Node x1 (t2::c1)
      else Node x2 (t1::c2)

insertTree : Tree -> InternalHeap -> InternalHeap
insertTree t ts = let r = rank t in
  case ts of
    []      -> [(r, t)]
    (r', t')::ts' ->
      if r== r' then insertTree (link t t') ts'
      else if r < r' then (r, t) :: ts
      else Debug.crash "insertTree: impossible"

merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (((r1, n1) as t1)::ts1', ((r2, n2) as t2)::ts2') ->
    if r1 < r2 then t1 :: merge_ ts1' ts2
    else if r2 < r1 then t2 :: merge_ ts2' ts1
    else insertTree (link n1 n2) (merge_ ts1' ts2')

attachRank : Tree -> (Rank, Tree)
attachRank t = (rank t, t)

removeMinTree : InternalHeap -> (Tree, InternalHeap)
removeMinTree ts = case ts of
  []     -> Debug.crash "removeMinTree: impossible"
  [(_, t)]    -> (t, [])
  ((_, t) as t1)::ts' ->
    let (t',ts'') = removeMinTree ts' in
    if root t < root t'
      then (t, ts')
      else (t', t1::ts'')

{-- External Interface --------------------------------------------------}

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x (WrapHeap ts) = WrapHeap (insertTree (Node x []) ts)

merge : Heap -> Heap -> Heap
merge (WrapHeap ts1) (WrapHeap ts2) = WrapHeap (merge_ ts1 ts2)

findMin : Heap -> Maybe Int
findMin (WrapHeap ts) =
  case ts of
    [] -> Nothing
    _  -> Just (root (fst (removeMinTree ts)))

deleteMin : Heap -> Maybe Heap
deleteMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> let (Node x ts1, ts2) = removeMinTree ts in
        Just (WrapHeap (merge_ (List.reverse <| L.map attachRank <| ts1) ts2))
