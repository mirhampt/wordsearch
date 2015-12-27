module Component.GenerateOptions (Action, Model, init, update, view) where

{-| Component to display options for generating a puzzle. Uses the "elm
architecture."

# Types
@docs Action, Model

# Standard Functions
@docs init, update, view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import PuzzleLogic exposing (Difficulty(Easy, Moderate, Hard))


{-| Represents all the options currently selected on the form.
-}
type alias Model =
    { difficulty: Difficulty }


{-| Possible actions for the form. These likely won't need to be used directly
outside this module.
-}
type Action
    = UpdateDifficulty Difficulty


{-| Initialize the form's model with some sensible defaults.
-}
init : Model
init =
    { difficulty = Easy }


{-| Update the state of the component with the given action.
-}
update : Action -> Model -> Model
update action model =
    case action of
        UpdateDifficulty difficulty ->
            { model | difficulty = difficulty }


{-| Render the form.
-}
view : Signal.Address Action -> Model -> Html
view address model =
    -- size
    -- words?
    div [ class "controls" ]
        [ span [ class "difficulty" ]
            [ difficultyRadio address "Easy" Easy (model.difficulty == Easy)
            , difficultyRadio address "Moderate" Moderate (model.difficulty == Moderate)
            , difficultyRadio address "Hard" Hard (model.difficulty == Hard)
            ]
        ]


-- Render helpers.

difficultyRadio : Signal.Address Action -> String -> Difficulty -> Bool -> Html
difficultyRadio address labelText difficulty isChecked =
    label []
        [ input
            [ type' "radio"
            , checked isChecked
            , on "change" targetChecked (\_ -> Signal.message address (UpdateDifficulty difficulty))
            ] []
        , text labelText
        ]


control : String -> Html -> Html
control lbl html =
    label []
        [ text lbl
        , html ]
