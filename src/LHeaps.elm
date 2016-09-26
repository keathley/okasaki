module LHeaps exposing
  ( Heap
  , singleton
  , fromList
  , empty
  , isEmpty
  , findMin
  , insert
  , deleteMin
  )

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

empty : Heap
empty = E

singleton : Int -> Heap
singleton x = T 1 x E E

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x h = merge (singleton x) h

deleteMin : Heap -> Maybe Heap
deleteMin h = case h of
  E         -> Nothing
  T _ _ a b -> Just (merge a b)

findMin : Heap -> Maybe Int
findMin h = case h of
  T _ x _ _ -> Just x
  E         -> Nothing

fromList : List Int -> Heap
fromList xs =
  xs
  |> List.map singleton
  |> mergeHeaps

mergePairs : List Heap -> List Heap
mergePairs hs = case hs of
  []               -> []
  [h]              -> [h]
  (x :: y :: rest) -> merge x y :: mergePairs rest

mergeHeaps : List Heap -> Heap
mergeHeaps heaps = case heaps of
  []  -> empty
  [h] -> h
  _   -> heaps |> mergePairs |> mergeHeaps
