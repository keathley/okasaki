module BinomialHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

type alias Rank = Int
type Tree = Node Rank Int (List Tree)

type alias InternalHeap = List Tree
type Heap = WrapHeap InternalHeap

rank (Node r _ _) = r
root (Node _ x _) = x

link : Tree -> Tree -> Tree
link t1 t2 =
  let (Node r x1 c1) = t1
      (Node _ x2 c2) = t2
  in
    if x1 <= x2
      then Node (1+r) x1 (t2::c1)
      else Node (1+r) x2 (t1::c2)

insertTree : Tree -> InternalHeap -> InternalHeap
insertTree t ts = case ts of
  []      -> [t]
  t'::ts' ->
    if rank t == rank t' then insertTree (link t t') ts'
    else if rank t < rank t' then t :: ts
    else Debug.crash "insertTree: impossible"

merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1', t2::ts2') ->
    if rank t1 < rank t2 then t1 :: merge_ ts1' ts2
    else if rank t2 < rank t1 then t2 :: merge_ ts2' ts1
    else insertTree (link t1 t2) (merge_ ts1' ts2')

removeMinTree : InternalHeap -> (Tree, InternalHeap)
removeMinTree ts = case ts of
  []     -> Debug.crash "removeMinTree: impossible"
  [t]    -> (t, [])
  t::ts' ->
    let (t',ts'') = removeMinTree ts' in
    if root t < root t'
      then (t, ts')
      else (t', t::ts'')

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x (WrapHeap ts) = WrapHeap (insertTree (Node 0 x []) ts)

merge : Heap -> Heap -> Heap
merge (WrapHeap ts1) (WrapHeap ts2) = WrapHeap (merge_ ts1 ts2)

findMin0 : Heap -> Maybe Int
findMin0 (WrapHeap ts) =
  case List.map root ts of
    []    -> Nothing
    n::ns -> Just (List.foldl min n ns)

findMin : Heap -> Maybe Int
findMin (WrapHeap ts) =
  case ts of
    [] -> Nothing
    _  -> Just (root (fst (removeMinTree ts)))

deleteMin : Heap -> Maybe Heap
deleteMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> let (Node _ x ts1, ts2) = removeMinTree ts in
        Just (WrapHeap (merge_ (List.reverse ts1) ts2))


-- Alternative definition of merge_, adapted from:
--
-- http://stackoverflow.com/questions/11462626/
--   should-melding-merging-of-binomial-heaps-be-done-in-one-pass-or-two

merge' : InternalHeap -> InternalHeap -> InternalHeap
merge' ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1', t2::ts2') ->
    if rank t1 < rank t2 then t1 :: merge' ts1' ts2
    else if rank t2 < rank t1 then t2 :: merge' ts1 ts2'
    else merge_wc (link t1 t2) ts1' ts2'

merge_wc : Tree -> InternalHeap -> InternalHeap -> InternalHeap
merge_wc t ts1 ts2 = case (ts1, ts2) of
  ([], _) -> insertTree t ts2
  (_, []) -> insertTree t ts1
  (t1::ts1', t2::ts2') ->
    let (r,r1,r2) = (rank t, rank t1, rank t2) in
    if r <  r1 && r <  r2 then t :: merge' ts1 ts2
    else if r <  r1 && r == r2 then merge_wc (link t t2) ts1 ts2'
    else if r == r1 && r <  r2 then merge_wc (link t t1) ts1' ts2
    else if r == r1 && r == r2 then t :: merge_wc (link t1 t2) ts1' ts2'
    -- else if r == r1 && r == r2 then merge_wc (link t t1) ts1' ts2
    else Debug.crash "merge_wc: impossible"
