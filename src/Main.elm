module Main (main) where

import Random
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Effects exposing (Effects)
import StartApp
import Board exposing (Board)
import Component.Board
import Component.GenerateOptions
import PuzzleLogic


type alias Model =
    { board : Board
    , seed : Random.Seed
    , options : Component.GenerateOptions.Model
    }


type Action
    = NoOp
    | GeneratePuzzle
    | Board Component.Board.Action
    | GenerateOptions Component.GenerateOptions.Action


init : (Model, Effects Action)
init =
    let
        model = { board = Component.Board.init(Board.makeEmpty 8 8)
                , seed = Random.initialSeed 1
                , options = Component.GenerateOptions.init
                }
    in
        (model, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoOp ->
            (model, Effects.none)

        GeneratePuzzle ->
            let
                width = Component.GenerateOptions.getWidth model.options
                height = Component.GenerateOptions.getHeight model.options
                difficulty = Component.GenerateOptions.getDifficulty model.options
                words = Component.GenerateOptions.getWords model.options
                emptyBoard = Board.makeEmpty width height
            in
                case PuzzleLogic.generate model.seed difficulty words emptyBoard of
                    (Ok board, seed) ->
                        ({ model
                            | seed = seed
                            , board = Component.Board.update (Component.Board.UpdateBoard board) model.board
                            }, Effects.none)

                    (Err _, _) ->
                        (model, Effects.none)

        Board action ->
            let
                model = { model |
                    board = Component.Board.update action model.board
                    }
            in
                (model, Effects.none)

        GenerateOptions action ->
            let
                model = { model |
                    options = Component.GenerateOptions.update action model.options
                    }
            in
                (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ div [ id "controls" ]
            [ Component.GenerateOptions.view
                (Signal.forwardTo address GenerateOptions)
                model.options
            , button [ onClick address GeneratePuzzle ] [ text "Generate" ]
            ]
        , Component.Board.view (Signal.forwardTo address Board) model.board
        ]


app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , view = view
        , update = update
        , inputs = []
        }


main : Signal Html
main =
    app.html
