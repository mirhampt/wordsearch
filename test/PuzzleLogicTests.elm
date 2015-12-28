module PuzzleLogicTests where

{-| Tests for PuzzleLogic module.
-}

import ElmTest exposing (..)
import Array
import Random exposing (Seed)
import Board exposing (Board)
import PuzzleLogic


all : Test
all =
    suite "Tests for PuzzleLogic module"
        [ testPlaceWordsSuccess
        , testPlaceWordsFailure
        , testPlaceWordsReturnsNewSeed
        , testFillInSquaresFillsEmptySquares
        , testFillInSquaresReturnsNewSeed
        , testGenerateSuccess
        ]


initialSeed : Seed
initialSeed =
    Random.initialSeed 1


emptyBoard : Board
emptyBoard =
    Board.makeEmpty 4 4


-- Expected result of calling placeWords with initialSeed.
expectedBoard : Board
expectedBoard =
    let
        board = Board.makeEmpty 4 4
    in
        { board |
            content = Array.fromList
                [ 'h', 'o', 'l', 'a'
                , ' ', ' ', ' ', ' '
                , ' ', ' ', ' ', ' '
                , 'f', 'o', 'u', 'r'
                ]
        }


-- Expected result of calling fillInSquares with initialSeed.
expectedFilledInBoard : Board
expectedFilledInBoard =
    let
        board = Board.makeEmpty 4 4
    in
        { board |
            content = Array.fromList
                [ 'h', 'o', 'l', 'a'
                , 'u', 'w', 'n', 'g'
                , 'y', 'z', 'k', 'n'
                , 'f', 'o', 'u', 'r'
                ]
        }


-- Expected result of calling generate with initialSeed.
expectedGeneratedBoard : Board
expectedGeneratedBoard =
    let
        board = Board.makeEmpty 4 4
    in
        { board |
            content = Array.fromList
                [ 'h', 'o', 'l', 'a'
                , 'f', 'c', 'q', 'v'
                , 'r', 'l', 'x', 'e'
                , 'f', 'o', 'u', 'r'
                ]
        }


testPlaceWordsSuccess : Test
testPlaceWordsSuccess =
    let
        words = ["hola", "four"]
        (board, _) = PuzzleLogic.placeWords initialSeed PuzzleLogic.Easy words emptyBoard
    in
        test "placeWords places the given words on the board"
            (assertEqual board (Ok expectedBoard))


testPlaceWordsFailure : Test
testPlaceWordsFailure =
    let
        words = ["toolong", "forthispuzzle"]
        (board, _) = PuzzleLogic.placeWords initialSeed PuzzleLogic.Easy words emptyBoard
    in
        test "placeWords fails if words are too long"
            (assertEqual board (Err "Could not place the word 'toolong' in the puzzle."))


testPlaceWordsReturnsNewSeed : Test
testPlaceWordsReturnsNewSeed =
    let
        words = ["hola", "four"]
        (_, seed) = PuzzleLogic.placeWords initialSeed PuzzleLogic.Easy words emptyBoard
    in
        test "placeWords returns a new random seed"
            (assertNotEqual seed initialSeed)


testFillInSquaresFillsEmptySquares : Test
testFillInSquaresFillsEmptySquares =
    let
        (board, _) = PuzzleLogic.fillInSquares initialSeed expectedBoard
    in
        test "fillInSquares fills in empty squares with characters"
            (assertEqual board expectedFilledInBoard)


testFillInSquaresReturnsNewSeed : Test
testFillInSquaresReturnsNewSeed =
    let
        (_, seed) = PuzzleLogic.fillInSquares initialSeed expectedBoard
    in
        test "fillInSquares returns a new random seed"
            (assertNotEqual initialSeed seed)


testGenerateSuccess : Test
testGenerateSuccess =
    let
        words = ["hola", "four"]
        (board, _) = PuzzleLogic.generate initialSeed PuzzleLogic.Easy words emptyBoard
    in
        test "generate successfully creates a full puzzle"
            (assertEqual board (Ok (expectedGeneratedBoard, expectedBoard)))
