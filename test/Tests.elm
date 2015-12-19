module Tests where

import ElmTest exposing (..)
import BoardTests
import PuzzleLogicTests


all : Test
all =
    suite "Tests for word search"
        [ BoardTests.all
        , PuzzleLogicTests.all
        ]
