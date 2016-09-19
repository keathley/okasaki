module ListsAndTrees where

suffixes xs =
  -- TODO
  []

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem _ _ =
  -- TODO
  False

fullTree : Int -> Int -> Tree
fullTree x h =
  if h < 1 then
      Empty
  else
      let s = fullTree x (h - 1) in
      Node x s s

balancedTree : Int -> Int -> Tree
balancedTree x n =
  if n < 1 then
      Empty
  else if n == 1 then
      Node x Empty Empty
  else if (n % 2) == 1 then
      let s = balancedTree x (n // 2) in
      Node x s s
  else
      let (l, r) = create2 x ((n - 1) // 2) in
      Node x l r

create2 : Int -> Int -> (Tree, Tree)
create2 x m =
  (balancedTree x m, balancedTree x (m + 1))
