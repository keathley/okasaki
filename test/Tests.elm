module Tests where

import ElmTest exposing (..)

import FPTest
import ListsAndTreesTest

all : Test
all =
    suite "Homework Problems"
        [
            FPTest.tests,
            ListsAndTreesTest.tests
        ]
