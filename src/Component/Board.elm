module Component.Board (Model, Action(UpdateBoard), init, update, view) where

{-| Component to display a Board. Uses the "elm architecture."

Along with displaying the board, there will be an option to display only the
words without the blank squares filled with characters. For this reason,
the `UpdateBoard` event takes two arguments: the board completely filled in
and the board with just the words placed and no random characters inserted.

# Actions
@docs Action

# Standard Functions
@docs init, update, view
-}

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import Board exposing (Board)


type alias Model =
    { board : Board
    , justWordsBoard : Board
    , showWords : Bool
    }


{-| Actions for the component:

* UpdateBoard Board Board: Updates the compenent's model with the given board.
    The second argument should be the generated board without the blank spaces
    filled in with random characters.
* ShowWords Bool: If true, the component will render the board without the
    blanks spaces filled in to make it obvious where the words have been placed.
-}
type Action
    = UpdateBoard Board Board
    | ShowWords Bool


{-| Initialize the component and returns it's model.
-}
init : Board -> Model
init board =
    { board = board
    , justWordsBoard = board
    , showWords = False }


{-| Process the events for the component. See the documentation for Action.
-}
update : Action -> Model -> Model
update action model =
    case action of
        UpdateBoard board justWordsBoard ->
            { model
                | board = board
                , justWordsBoard = justWordsBoard
                }

        ShowWords showWords ->
            { model | showWords = showWords }


--  Basic styles.

tableStyle : Attribute
tableStyle = style
    [ ("border-collapse", "collapse")
    , ("text-align", "center")
    ]


tdStyle : Attribute
tdStyle = style
    [ ("width", "30px")
    , ("height", "30px")
    , ("border", "1px solid #bbb")
    ]


{-| Render the board.
-}
view : Signal.Address Action -> Model -> Html
view address model =
    let
        board = if model.showWords then model.justWordsBoard else model.board
    in
        div []
            [ label []
                [ input
                    [ type' "checkbox"
                    , checked model.showWords
                    , on "change" targetChecked (Signal.message address << ShowWords)] []
                , text "Just show the words"
                ]
            , table [ class "board", tableStyle ]
                (List.map renderRow (Board.getRows board))
            ]


renderRow : List Char -> Html
renderRow row =
    tr [] (List.map (\c -> td [ tdStyle ] [ text (String.fromChar c) ]) row)
