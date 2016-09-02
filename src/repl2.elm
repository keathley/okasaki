import ListsAndTrees as LT exposing (..)
import FP as FP exposing (..)

LT.suffixes [1..4]

LT.fullTree 1 2

LT.balancedTree 1 5

LT.create2 1 3

List.map (List.length << balancedTrees 0) [0..20]

List.map (List.length << completeTrees 0) [0..5]

FP.subsequences [1, 2, 3]

List.map (flip (::) []) [1, 2, 3]

List.length <| LT.combinations 5 [Empty, LT.leaf 1]

List.map (List.length << almostCompleteTrees 0) [0..4]

LT.part 2 [1, 2, 3, 4, 5]
