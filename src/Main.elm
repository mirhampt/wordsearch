module Main where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Board exposing (Board)


main : Html
main =
    renderBoard (Board.makeEmpty 3 3)


renderBoard : Board -> Html
renderBoard board =
    table [ class "board" ]
        (List.map renderRow (Board.getRows board))


renderRow : List Char -> Html
renderRow row =
    tr [] (List.map (\c -> td [] [ text (String.fromChar c) ]) row)
