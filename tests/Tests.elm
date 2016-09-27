module Tests exposing (all)

import Test exposing (..)
import FPTest
import ListsAndTreesTest
import LHeapsTest

all : Test
all =
    describe "Homework Problems"
        [
         -- HW 1
            FPTest.tests,
         -- HW 2
            ListsAndTreesTest.tests,
         -- HW 3
            LHeapsTest.tests
        ]
