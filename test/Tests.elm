module Tests where

import ElmTest exposing (..)

import FPTest

all : Test
all =
    suite "Homework Problems"
        [
            FPTest.tests
        ]
