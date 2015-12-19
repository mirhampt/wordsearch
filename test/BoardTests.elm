module BoardTests where

{-| Tests for Board module.
-}

import Board exposing (Board)
import ElmTest exposing (..)
import Array


all : Test
all =
    suite "Tests for Board module"
        [ testMakeEmptySize
        , testGetInBounds
        , testGetRowOutOfBounds
        , testGetColOutOfBounds
        , testSetInBounds
        , testSetRowOutOfBounds
        , testSetColOutOfBounds
        ]


emptyBoard : Board
emptyBoard =
    Board.makeEmpty 2 2


testMakeEmptySize : Test
testMakeEmptySize =
    test "makeEmpty sets the content array correctly"
        (assertEqual (emptyBoard |> .content |> Array.length) 4)


testGetInBounds : Test
testGetInBounds =
    test "get returns the character if indexes are in bounds"
        (assertEqual (emptyBoard |> Board.get 0 0) (Just ' '))


testGetRowOutOfBounds : Test
testGetRowOutOfBounds =
    test "get returns Nothing if a row index is out of bounds"
        (assertEqual (emptyBoard |> Board.get 2 0) Nothing)


testGetColOutOfBounds : Test
testGetColOutOfBounds =
    test "get returns Nothing if a column index is out of bounds"
        (assertEqual (emptyBoard |> Board.get 0 2) Nothing)


testSetInBounds : Test
testSetInBounds =
    test "set replaces the character at the indexes"
        (assertEqual (emptyBoard |> Board.set 0 0 'x' |> Board.get 0 0) (Just 'x'))


testSetRowOutOfBounds : Test
testSetRowOutOfBounds =
    test "set returns unchanged board if row index is out of bounds"
        (assertEqual (emptyBoard |> Board.set 2 0 'x') emptyBoard)


testSetColOutOfBounds : Test
testSetColOutOfBounds =
    test "set returns unchanged board if column index is out of bounds"
        (assertEqual (emptyBoard |> Board.set 0 2 'x') emptyBoard)
