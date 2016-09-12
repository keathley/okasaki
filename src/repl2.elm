import ListsAndTrees as LT exposing (..)
import FP as FP exposing (..)

LT.suffixes [1..4]

LT.mem 1 (Node 0 Empty (leaf 1))

LT.fullTree 1 2

LT.balancedTree 1 5

LT.create2 1 3

List.map (List.length << balancedTrees 0) [0..20]

List.map (List.length << completeTrees 0) [0..5]

List.map (List.length << almostCompleteTrees 0) [0..5]

