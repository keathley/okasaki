module ListsAndTrees where

import List exposing (..)
import Maybe exposing (withDefault)

split : Int -> List a -> (List a, List a)
split n l = (take n l, drop n l)

chunks : Int -> List a -> List (List a)
chunks n xs =
   case split n xs of
      (p, []) -> [p]
      (p, q)  -> p::chunks n q

suffixes xs =
   case xs of
      []    -> [[]]
      _::ss -> xs :: suffixes ss

pairs : List a -> List a -> List (a, a)
pairs l1 l2 =
   let f = (\l x -> map2 (,) l (repeat (length l) x))
   in concatMap (f l1) l2

cartesian : Int -> List a -> List (List a)
cartesian n s =
   if n == 0 then
      [[]]
   else
      let f x = map ((::) x) <| cartesian (n - 1) s
      in concatMap f s

type Tree = Empty | Node Int Tree Tree

leaf : Int -> Tree
leaf x = Node x Empty Empty

mem : Int -> Tree -> Bool
mem x t =
   let memc t c = case (t, c) of
                     (Empty, Empty)       -> False
                     (Empty, Node y _ _)  -> x == y
                     (Node y l r, _)      -> if x < y then
                                                memc l c
                                             else
                                                memc r t
   in memc t Empty

fullTree : Int -> Int -> Tree
fullTree x h =
   if h <= 0 then
      Empty
   else
      let c = fullTree x (h - 1)
      in Node x c c

balancedTrees : Int -> Int -> List Tree
balancedTrees x s =
   if s <= 0 then
      [Empty]
   else if s == 1 then
      [leaf x]
   else if s % 2 == 1 then
      let ts = balancedTrees x ((s - 1) // 2)
      in map (\(l, r) -> Node x l r)
             <| pairs ts ts
   else
      let ls = balancedTrees x ((s - 1) // 2)
          rs = balancedTrees x (s // 2)
      in concatMap (\(l, r) -> [Node x l r, Node x r l])
                   (pairs ls rs)

balancedTree : Int -> Int -> Tree
balancedTree x s = balanced x s |> fst

balanced : Int -> Int -> (Tree, Tree)
balanced x s =
   if s <= 0 then
      (Empty, leaf x)
   else if s % 2 == 1 then
      let (l, r) = balanced x (s // 2)
      in (Node x l l, Node x l r)
   else
      let (l, r) = balanced x (s // 2 - 1)
      in (Node x l r, Node x r r)

completeTrees : Int -> Int -> List Tree
completeTrees x h =
   let l = leaf x
       h = h - 1
       f = fullTree x h
       n = 2^h - 1
   in scanl (addLeaf h) f (repeat n l)

addLeaf : Int -> Tree -> Tree -> Tree
addLeaf h n t =
   if h <= 1 then
      case t of
         Empty          -> n
         Node y Empty r -> Node y n r
         Node y l Empty -> Node y l n
         _              -> Empty
   else
      case t of
         Empty      ->  addLeaf (h - 1) n t
         Node y l r ->  case addLeaf (h - 1) n l of
                           Empty -> case addLeaf (h - 1) n r of
                                       Empty -> Empty
                                       r     -> Node y l r
                           l     -> Node y l r

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees x h =
   if h == 0 then
      [Empty]
   else
      let n = 2 ^ (h - 1)
          f = repeat (n - 1) <| leaf x
          l = drop 1 <| cartesian n [Empty, leaf x]
      in map (withDefault Empty << head << almostComplete 1 << ((++) f)) l

almostComplete : Int -> List Tree -> List Tree
almostComplete depth list =
   case list of
      [] -> []
      _  -> let (parents, descends) = split depth list
                depth2   = depth * 2
                children = almostComplete depth2 descends ++ repeat depth2 Empty
                join parent children = case (parent, children) of
                                          (Node x _ _, l::r::[]) -> Node x l r
                                          _                      -> Empty
            in map2 join parents <| chunks 2 children
