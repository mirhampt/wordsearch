-- module PuzzleLogic (Difficulty(Easy, Moderate, Hard), placeWords) where
module PuzzleLogic (..) where

{-| This module handles the main logic of generating a word search puzzle.
-}

import Board exposing (Board)
import Random exposing (Seed)
import ListUtil
import String
import Array
import Char


{-| The difficulty of the puzzle to generate.

* `Easy`: Words will only be placed left-to-right and top-to-bottom.
* `Moderate`: In addition to left-to-right and top-to-bottom, words may be
    placed diagonally in the topleft-to-bottomright or bottomleft-to-topright
    direction.
* `Hard`: Words may appear in any direction and may be backwards.
-}
type Difficulty
    = Easy
    | Moderate
    | Hard


{-| The possible directions to place a word on the puzzle.
-}
type Direction
    = LeftRight
    | RightLeft
    | TopDown
    | BottomUp
    | LeftRightTopBottom
    | LeftRightBottomTop
    | RightLeftTopBottom
    | RightLeftBottomTop


{-| Fully generate a puzzle by randomly placing the words taking the difficulty
into account, then filling in the remaining blanks with random characters.

The board passed to this function should be blank.
-}
generate : Seed -> Difficulty -> List String -> Board -> (Result String Board, Seed)
generate seed difficulty words board =
    case placeWords seed difficulty words board of
        (Ok newBoard, newSeed) ->
            let
                (filledBoard, newSeed) = fillInSquares newSeed newBoard
            in
                (Ok filledBoard, newSeed)

        otherwise ->
            otherwise


{-| Randomly places the words onto the board.
-}
placeWords : Seed -> Difficulty -> List String -> Board -> (Result String Board, Seed)
placeWords seed difficulty words board =
    let
        (positions, newSeed) =
            getPossiblePositions board
                |> ListUtil.shuffle seed
    in
        recurPlaceWords seed words positions difficulty board


recurPlaceWords : Seed -> List String -> List (Int, Int) -> Difficulty -> Board -> (Result String Board, Seed)
recurPlaceWords seed words positions difficulty board =
    let
        (directions, newSeed) =
            getDirections difficulty
                |> ListUtil.shuffle seed
    in
        case words of
            [] ->
                -- All words placed.
                (Ok board, newSeed)

            word::wordTail ->
                case positions of
                    [] ->
                        -- We have run out of possible positions.
                        (Err ("Could not place the word '" ++ word ++ "' in the puzzle."), newSeed)

                    (row, col)::posTail ->
                        case tryEachDirection row col directions word board of
                            Ok newBoard ->
                                -- Word placed.
                                recurPlaceWords newSeed wordTail posTail difficulty newBoard

                            Err _ ->
                                -- Word wouldn't fit at this position. Try it at the next position.
                                recurPlaceWords newSeed (word::wordTail) posTail difficulty board


{-| Fill in the blank squares with random characters.
-}
fillInSquares : Seed -> Board -> (Board, Seed)
fillInSquares seed board =
    let
        foldStep char (list, seed) =
            case char of
                ' ' ->
                    let
                        (newChar, newSeed) = Random.generate characterGenerator seed
                    in
                        (newChar::list, newSeed)

                _ ->
                    (char::list, seed)

        (content, newSeed) = Array.foldr foldStep ([], seed) board.content
    in
        ({ board | content = Array.fromList content }, newSeed)


{-| A random lowercase, alphabetic character generator.
-}
characterGenerator : Random.Generator Char
characterGenerator = Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


{-| Place a word on the board in the given direction starting at the given
position.

This will check each square to make sure that it isn't overwriting another
letter (unless it is the same letter needed for the word being inserted).
-}
placeWord : Int -> Int -> Direction -> String -> Board -> Result String Board
placeWord row col direction word board =
    case String.toList word of
        [] ->
            Ok board

        char::rest ->
            let
                (rowDelta, colDelta) = getDirectionDelta direction
                squareVal = Board.get row col board
                newBoard = Board.set row col char board
            in
                if squareVal == Just ' ' || squareVal == Just char
                    then
                        placeWord (row + rowDelta) (col + colDelta) direction (String.fromList rest) newBoard
                    else
                        Err ("Unable to place the word '" ++ word ++ "' in the puzzle.")


{-| Try placing the word at the given position in each direction given in order.
Once a successful direction is found, the updated board will be returned.
-}
tryEachDirection : Int -> Int -> List Direction -> String -> Board -> Result String Board
tryEachDirection row col directions word board =
    case directions of
        [] ->
            -- Ran out of possible directions.
            Err ("Unable to place '" ++ word ++ "' in the puzzle.")

        direction::rest ->
            case placeWord row col direction word board of
                Err _ ->
                    tryEachDirection row col rest word board

                Ok newBoard ->
                    Ok newBoard


{-| Returns a (row, column) delta for the given direction.
-}
getDirectionDelta : Direction -> (Int, Int)
getDirectionDelta direction =
    case direction of
        LeftRight ->
            (0, 1)

        RightLeft ->
            (0, -1)

        TopDown ->
            (1, 0)

        BottomUp ->
            (-1, 0)

        LeftRightTopBottom ->
            (1, 1)

        LeftRightBottomTop ->
            (-1, 1)

        RightLeftTopBottom ->
            (1, -1)

        RightLeftBottomTop ->
            (-1, -1)


{-| Get the list of word directions to use for the given difficulty.
-}
getDirections : Difficulty -> List Direction
getDirections difficulty =
    case difficulty of
        Easy ->
            [ LeftRight
            , TopDown
            ]

        Moderate ->
            [ LeftRight
            , TopDown
            , LeftRightTopBottom
            , LeftRightBottomTop
            ]

        Hard ->
            [ LeftRight
            , TopDown
            , LeftRightTopBottom
            , LeftRightBottomTop
            , RightLeft
            , BottomUp
            , RightLeftTopBottom
            , RightLeftBottomTop
            ]


{-| Return all possible indexes for the given board.
-}
getPossiblePositions : Board -> List (Int, Int)
getPossiblePositions board =
    -- The Cartesian Product of the board's column and row sets.
    List.concatMap (\row ->
        List.map (\col ->
            (row, col)) [0..board.width-1]) [0..board.height-1]
