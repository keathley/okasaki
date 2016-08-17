module Persistence where

find : a -> List a -> Bool
find x xs = case xs of
  []     -> False
  y::xs' -> if x == y then True else find x xs'

update : List a -> Int -> a -> List a
update xs i y =
  case (xs, i) of
    ([],     _) -> []
    (x::xs', 0) -> y :: xs'
    (x::xs', _) -> x :: update xs' (i-1) y

append : List a -> List a -> List a
append xs ys =
  case xs of
    []     -> ys
    x::xs' -> x :: append xs' ys

type Tree a = Empty | Node a (Tree a) (Tree a)

findBT : a -> Tree a -> Bool
findBT x t = case t of
  Empty             -> False
  Node y left right -> x == y || findBT x left || findBT x right

findBST : comparable -> Tree comparable -> Bool
findBST x t = case t of
  Empty -> False
  Node y left right ->
    if x == y then      True
    else if x < y then  findBST x left
    else {- x > y -}    findBST x right

insert : comparable -> Tree comparable -> Tree comparable
insert x t = case t of
  Empty -> Node x Empty Empty
  Node y left right ->
    if x == y then      t
    else if x < y then  Node y (insert x left) right
    else {- x > y -}    Node y left (insert x right)
