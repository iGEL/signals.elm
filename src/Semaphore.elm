module Semaphore exposing (..)

import Html exposing (div, button, text, table, tr, th, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal
import Messages exposing (..)


type Msg
    = FirstSignalMsg Messages.Msg
    | SecondSignalMsg Messages.Msg
    | SwitchLanguage Language


type Language
    = German
    | English


type alias Model =
    { distantSignal : Signal.Model
    , signalRepeater : Signal.Model
    , combinationSignal : Signal.Model
    , mainSignal : Signal.Model
    , language : Language
    }


init : Model
init =
    { distantSignal = Signal.distantSignal
    , signalRepeater = Signal.signalRepeater
    , combinationSignal = Signal.combinationSignal
    , mainSignal = Signal.mainSignal
    , language = German
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstSignalMsg signalmsg ->
            let
                newDistantSignal =
                    Signal.update (ToDistantSignal signalmsg) model.distantSignal

                newSignalRepeater =
                    Signal.update (ToDistantSignal signalmsg) model.signalRepeater

                newCombinationSignal =
                    Signal.update (ToMainSignal signalmsg) model.combinationSignal
            in
                { model
                    | distantSignal = newDistantSignal
                    , signalRepeater = newSignalRepeater
                    , combinationSignal = newCombinationSignal
                }

        SecondSignalMsg signalmsg ->
            let
                newCombinationSignal =
                    Signal.update (ToDistantSignal signalmsg) model.combinationSignal

                newMainSignal =
                    Signal.update (ToMainSignal signalmsg) model.mainSignal
            in
                { model
                    | combinationSignal = newCombinationSignal
                    , mainSignal = newMainSignal
                }

        SwitchLanguage language ->
            { model | language = language }


translate model german english =
    case model.language of
        German ->
            text german

        English ->
            text english


view : Model -> Html.Html Msg
view model =
    div []
        [ button [ onClick (SwitchLanguage German) ] [ text "Deutsch" ]
        , button [ onClick (SwitchLanguage English) ] [ text "English" ]
        , table [ style [ ( "margin", "20px" ) ] ]
            [ tr []
                [ th [] [ translate model "Vorsignal" "Distant Signal" ]
                , th [] [ translate model "Vorsignalwiederholer" "Signal Repeater" ]
                , th [] [ translate model "Mehrabschnittssignal" "Combination Signal" ]
                , th [] [ translate model "Hauptsignal" "Main Signal" ]
                ]
            , tr []
                [ td [] []
                , td [] []
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick (FirstSignalMsg Stop) ]
                        [ translate model "Halt" "Stop" ]
                    , button [ onClick (FirstSignalMsg Proceed) ]
                        [ translate model "Fahrt" "Proceed" ]
                    ]
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick (SecondSignalMsg Stop) ]
                        [ translate model "Halt" "Stop" ]
                    , button [ onClick (SecondSignalMsg Proceed) ]
                        [ translate model "Fahrt" "Proceed" ]
                    ]
                ]
            , tr []
                [ td [] [ Html.map FirstSignalMsg (Signal.view model.distantSignal) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.signalRepeater) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.combinationSignal) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.mainSignal) ]
                ]
            ]
        ]


main =
    Html.beginnerProgram { model = init, view = view, update = update }
