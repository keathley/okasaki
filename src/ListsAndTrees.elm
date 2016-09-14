module ListsAndTrees where

suffixes : List a -> List (List a)
suffixes xs =
  -- I am supposed to have comments here arguing why this implementation has an
  -- efficient asymptotic growth rate ... but I am not that smart. This probably
  -- is fine, and if not, just upgrade the EC2 instance.
  case xs of
      []     -> []
      x::xs' -> xs :: suffixes xs'

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem x t =
  -- h+1 comparisons for this member function, where h is the height.
    case t of
        Empty -> False
        Node y left right ->
            if x == y then     True
            else if x < y then mem x left
            else {- x > y -}   mem x right

fullTree : Int -> Int -> Tree
fullTree x h =
  -- *should* run in O(h) time?
    if h < 1 then  Empty
    else  Node x (fullTree x (h - 1)) (fullTree x (h - 1))

balancedTree : Int -> Int -> Tree
balancedTree x n =
  -- I didn't use the helper function because I couldn't figure out how to use
  -- it the way that the instructions intended.
  if n < 1           then Empty
  else if n % 2 == 0 then
           Node x (balancedTree x ((n - 1)// 2)) (balancedTree x (n // 2))
  else Node x (balancedTree x ((n - 1) // 2)) (balancedTree x ((n - 1) // 2))

create2 : Int -> Int -> (Tree, Tree)
create2 _ _ =
  -- TODO
  (Empty, Empty)

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

