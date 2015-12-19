module Board (Board, makeEmpty, get, set) where

{-| Defines a board type and functions to manipulate it.

# Types
@docs Board

# Helpers
@docs makeEmpty, get, set
-}

import Array exposing (Array)


{-| Represents a word search puzzle board.

Instead of manipulating these values directly, use one of the helper methods
included in this module.
-}
type alias Board =
    { width : Int
    , height : Int
    , content : Array Char
    }


{-| Create a new empty puzzle board with the given dimensions.

    Board.makeEmpty 2 2  -- { width = 2, height = 2, ... } : Board.Board
-}
makeEmpty : Int -> Int -> Board
makeEmpty width height =
    let
        content = Array.repeat (width * height) ' '
    in
        { width = width, height = height, content = content }


{-| Get the character at the given position.

If row or col is out of bounds, returns `Nothing`.

    (Board.makeEmpty 2 2 |> Board.get 0 0) == Just ' '
    (Board.makeEmpty 2 2 |> Board.get 3 0) == Nothing
-}
get : Int -> Int -> Board -> Maybe Char
get row col board =
    if col >= board.width || col < 0
        then Nothing
        else Array.get (index row col board) board.content


{-| Sets a value on the board to the given character.

If row or col is out of bounds, the board will be returned unaltered.

    (Board.makeEmpty 2 2 |> Board.set 0 0 'c' |> Board.get 0 0) == Just 'c'
    (Board.makeEmpty 2 2 |> Board.set 3 0 'c') == Board.makeEmpty 2 2
-}
set : Int -> Int -> Char -> Board -> Board
set row col char board =
    if col >= board.width || col < 0
        then board
        else
            { board |
                content = Array.set (index row col board) char board.content
            }


{-| Given row and column indexes, return a single index into the Board.content
array.
-}
index : Int -> Int -> Board -> Int
index row col board =
    col + (row * board.width)
