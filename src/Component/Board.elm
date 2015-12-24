module Component.Board (Action(UpdateBoard), init, update, view) where

{-| Component to display a Board. Uses the "elm architecture."

For now, the board is simply a static display. Interactivity (such as
selecting words) may be added later.

# Actions
@docs Action

# Standard Functions
@docs init, update, view
-}

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Board exposing (Board)


type alias Model =
    Board


{-| Actions for the component:

* UpdateBoard Board: Updates the compenent's model with the given board.
-}
type Action
    = UpdateBoard Board


{-| Initialize the component and returns it's model.
-}
init : Board -> Model
init board =
    board


{-| Process the events for the component. See the documentation for Action.
-}
update : Action -> Model -> Model
update action model =
    case action of
        UpdateBoard board ->
            board


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
    table [ class "board", tableStyle ]
        (List.map renderRow (Board.getRows model))


renderRow : List Char -> Html
renderRow row =
    tr [] (List.map (\c -> td [ tdStyle ] [ text (String.fromChar c) ]) row)
