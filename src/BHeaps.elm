module BHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

type Tree = Node Int (List Tree)
type alias Rank = Int
type alias InternalHeap = List (Rank, Tree)
type Heap = WrapHeap InternalHeap


rank (Node _ l) = List.length l
root (Node x _) = x

link : Tree -> Tree -> Tree
link t1 t2 =
  let (Node x1 c1) = t1
      (Node x2 c2) = t2
  in
    if x1 <= x2
      then Node x1 (t2::c1)
      else Node x2 (t1::c2)

insertTree : Tree -> InternalHeap -> InternalHeap
insertTree t ts = case ts of
  []      -> [(rank t, t)]
  (r', t')::ts' ->
    if rank t == r' then insertTree (link t t') ts'
    else if rank t < r' then (rank t, t) :: ts
    else Debug.crash "insertTree: impossible"

merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  ((r1, t1)::ts1', (r2, t2)::ts2') ->
    if r1 < r2 then (r1, t1) :: merge_ ts1' ts2
    else if r2 < r1 then (r2, t2) :: merge_ ts2' ts1
    else insertTree (link t1 t2) (merge_ ts1' ts2')

removeMinTree : InternalHeap -> (Tree, InternalHeap)
removeMinTree ts = case ts of
  []       -> Debug.crash "removeMinTree: impossible"
  [(r, t)] -> (t, [])
  (r, t)::ts' ->
    let (t',ts'') = removeMinTree ts' in
    if root t < root t'
      then (t, ts')
      else (t', (r, t)::ts'')

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
        Just (WrapHeap (merge_ (List.map (\t -> (rank t, t))
                                         (List.reverse ts1))
                               ts2))
