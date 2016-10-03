module BHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

type Tree = Node Int (List Tree)
type alias Rank = Int
type alias InternalHeap = List (Rank, Tree)
type Heap = WrapHeap InternalHeap

{-- Internal Helpers ----------------------------------------------------}

rank (r, _) = r
root (_, (Node x _)) = x

link : (Rank, Tree) -> (Rank, Tree) -> (Rank, Tree)
link (r, t1) (_, t2) = case (t1, t2) of
    (Node x1 c1, Node x2 c2) ->
        (r + 1, if x1 <= x2 then Node x1 (t2 :: c1)
                else Node x2 (t1 :: c2))

insertTree : (Rank, Tree) -> InternalHeap -> InternalHeap
insertTree (r, t) ts = case ts of
  []              -> [(r, t)]
  (r', t') :: ts' ->
      if r == r' then insertTree (link (r, t) (r', t')) ts'
      else if r < r' then (r, t) :: (r', t') :: ts'
      else Debug.crash "insertTree: impossible"


merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1', t2::ts2') ->
  if rank t1 < rank t2 then t1 :: merge_ ts1' ts2
  else if rank t2 < rank t1 then t2 :: merge_ ts2' ts1
  else insertTree (link t1 t2) (merge_ ts1' ts2')

removeMinTree : InternalHeap -> ((Rank, Tree), InternalHeap)
removeMinTree ts = case ts of
  []     -> Debug.crash "removeMinTree: Impossible"
  [t]    -> (t, [])
  t::ts' -> let (t', ts'') = removeMinTree ts' in
            if root t < root t' then (t, ts')
            else (t', t::ts'')

{-- External Interface --------------------------------------------------}

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x (WrapHeap ts) = WrapHeap <| insertTree (0, Node x []) ts

merge : Heap -> Heap -> Heap
merge (WrapHeap ts1) (WrapHeap ts2) = WrapHeap (merge_ ts1 ts2)

findMin : Heap -> Maybe Int
findMin (WrapHeap ts) = case List.map root ts of
  [] -> Nothing
  n :: ns -> Just <| List.foldl min n ns

deleteMin : Heap -> Maybe Heap
deleteMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> let ((r, Node x ts1), ts2) = removeMinTree ts in
        let ts1' = (List.map (\t -> (r-1, t)) (List.reverse ts1)) in
        Just (WrapHeap (merge_ ts1' ts2))
