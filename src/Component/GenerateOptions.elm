module Component.GenerateOptions (Action, Model, init, update, view) where

{-| Component to display options for generating a puzzle. Uses the "elm
architecture."

# Types
@docs Action, Model

# Standard Functions
@docs init, update, view
-}

import Regex
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked, targetValue)
import PuzzleLogic exposing (Difficulty(Easy, Moderate, Hard))


{-| Represents all the options currently selected on the form.
-}
type alias Model =
    { difficulty : Difficulty
    , width : Int
    , height : Int
    , words : List String
    }


{-| Possible actions for the form. These likely won't need to be used directly
outside this module.
-}
type Action
    = UpdateDifficulty Difficulty
    | UpdateWidth Int
    | UpdateHeight Int
    | UpdateWords String


{-| Initialize the form's model with some sensible defaults.
-}
init : Model
init =
    { difficulty = Easy
    , width = 8
    , height = 8
    , words = []
    }


{-| Update the state of the component with the given action.
-}
update : Action -> Model -> Model
update action model =
    case action of
        UpdateDifficulty difficulty ->
            { model | difficulty = difficulty }

        UpdateWidth width ->
            { model | width = width }

        UpdateHeight height ->
            { model | height = height }

        UpdateWords wordsString ->
            -- Strip out non-alphabetic characters and make them all lowercase.
            let replaceNonAlpha = Regex.replace Regex.All (Regex.regex "[^a-z\\s]") (\_ -> " ")
                cleanAndSplit = String.words << replaceNonAlpha << String.toLower
            in
                { model | words = cleanAndSplit wordsString }


{-| Render the form.
-}
view : Signal.Address Action -> Model -> Html
view address model =
    -- size
    -- words?
    div [ class "controls" ]
        [ div [ class "difficulty" ]
            [ difficultyRadio address "Easy" Easy (model.difficulty == Easy)
            , difficultyRadio address "Moderate" Moderate (model.difficulty == Moderate)
            , difficultyRadio address "Hard" Hard (model.difficulty == Hard)
            ]
        , div [ class "size" ]
            [ control "Size" (span []
                [ sizeSelect address UpdateWidth [6..16] model.width
                , text "x"
                , sizeSelect address UpdateHeight [6..16] model.height
                ])
            ]
        , div [ class "words" ]
            [ control "Words" (textarea
                [ placeholder "Enter words here separated by spaces or new lines."
                , on "input" targetValue (Signal.message address << UpdateWords)
                ] [])
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


-- Generate a select field showing the size options in the given range.
-- 'action' should be one of: UpdateWidth or UpdateHeight.
sizeSelect : Signal.Address Action -> (Int -> Action) -> List Int -> Int -> Html
sizeSelect address action range selectedSize =
    let
        optionForValue val =
            option
                [ value val, selected (toString selectedSize == val) ]
                [ text val ]

        targetValAsInt =
            Result.withDefault selectedSize << String.toInt
    in
        select [ on "change" targetValue (Signal.message address << action << targetValAsInt) ]
            (range |> List.map toString |> List.map optionForValue)


control : String -> Html -> Html
control lbl html =
    label []
        [ text lbl
        , html ]
