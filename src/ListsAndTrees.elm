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
mem _ _ =
  -- TODO
  False

fullTree : Int -> Int -> Tree
fullTree _ _ =
  -- TODO
  Empty

balancedTree : Int -> Int -> Tree
balancedTree _ _ =
  -- TODO
  Empty

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

