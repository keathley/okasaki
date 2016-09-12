module ListsAndTrees where

-- import Debug as D
import List as L

-- One visit per list tail == O(n) time
-- One new cell allocation per tail (contents of the cell shared with the original) list == O(n) space
suffixes: List a -> List (List a)
suffixes xs =
  case xs of
    [] -> [[]]
    _::xs as l -> l :: suffixes xs

type Tree = Empty | Node Int Tree Tree

t : Tree
t = Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 6 Empty Empty)

mem2 : Int -> Tree -> Tree -> Bool
mem2 n t c =
  case t of
    Empty -> case c of
      Empty -> False
      Node x _ _ -> n == x
    Node x l r as candidate ->
      if n <= x then
        mem2 n l candidate
      else
        mem2 n r c

mem : Int -> Tree -> Bool
mem n t = case t of
  Empty -> False
  x -> mem2 n t x

treeBuilder : Int -> Int -> Tree -> Tree
treeBuilder d x t =
  if d == 0 then
    t
  else
    treeBuilder (d-1) x <| Node x t t

fullTree : Int -> Int -> Tree
fullTree x h =
  treeBuilder h x <| Node x Empty Empty

-- treeBuilder2 : Int -> Int -> Tree -> Tree
-- treeBuilder2 s x t =
--   case s of
--     0 -> t
--     1 -> Node x t Empty
--   else
--     treeBuilder (d-1) x <| Node x t t

foldNode : Int -> Int -> (Int, Tree) -> (Int, Tree)
foldNode x i (s, t) = let child = Node x Empty Empty
  in
    case t of
      Empty -> (s, t)
      Node x l r as node -> if i == s+2 then
        (s, Node x child r) else if i == s+3 then
          (s, Node x l child) else (s, node)

addChildren2 : Tree -> Int -> List Int -> Tree
addChildren2 t seed children =
  case t of
    Empty -> t
    Node x _ r as node -> let child = Node x Empty Empty
                              (_, tree) = L.foldl (foldNode x)
                                (seed, Node x Empty Empty) children
     in
      tree

addChildren : Tree -> Int -> List Int -> Tree
addChildren t seed children =
  case t of
    Empty -> t
    Node x Empty Empty -> addChildren2 t seed children
    Node x l r as node -> Node x (addChildren l (seed*2) children) (addChildren r (seed*2+1) children)

-- lastRow : Tree -> List Tree
-- lastRow t =
--   case t of
--     Empty -> t
--     Node _ Empty _ as leaf -> leaf
--     Node _ _ Empty as leaf -> leaf
--     Node _ l r -> lastRow l :: lastRow r

-- addRow : Tree -> Int -> Tree
-- addRow t n =
--   case n of
--     0 -> t
--     _ -> case t of
--       Empty -> t
--       Node _ Empty _ as leaf -> leaf
--       Node _ _ Empty as leaf -> leaf
--       Node _ l r -> addRow

-- Takes a final size for a tree and returns the list of all node indices
-- needed to fill out the leaf row and maintain a balanced tree.
-- Node indices are numbered as if performing a BFS on the tree.
neededIndices : Int -> List (List Int)
neededIndices s =
  case s of
    0 -> []
    _ ->
      let h = floor <| logBase 2 s
          lastRow = 2 ^ h
          leafRow = lastRow * 2
          diff = s - lastRow
          all = L.concatMap suffixes (suffixes [lastRow..(lastRow + (lastRow//2))])
      in
          L.filter (\l -> (not <| L.isEmpty l) && L.length l == diff//2 ||
            (diff//2 == 0 && L.length l == 1)) all
          -- from beginning of leaf row to middle, generate all suffixes, map them into tuples by adding leafRow / 2 to every item,
          -- filter for tuples whose combined list count == diff


balancedTree : Int -> Int -> Tree
balancedTree x n =
  let h = floor <| logBase 2 <| toFloat n
      diff = n - (2 ^ h)
  in
    if diff == 0 then
      fullTree x h
    else
      let lastRow = 2 ^ h
          t = fullTree x (h - 1)
      in
        t
  -- TODO

create2 : Int -> Int -> (Tree, Tree)
create2 s x =
  case s of
    0 -> (Empty, Node x Empty Empty)
    _ -> let children = create2 (s - 1) x
         in
            case children of
--              (Node _ l r as f, Node _ l2 r2 as s) -> (s, Node x r2 l2)
              (Node _ l r, Node _ l2 r2 as s) -> (Node x l2 <| Node x r Empty, Node x (Node x Empty r2) l2)
              (f, s) -> (Node x f Empty, Node x Empty s)
        --  in (snd children, Node x Empty <| snd children)

balancedTrees : Int -> Int -> List Tree
balancedTrees _ _ =
  -- TODO
  []

completeTrees : Int -> Int -> List Tree
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees _ _ =
  -- TODO
  []
