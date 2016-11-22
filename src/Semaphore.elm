module Semaphore exposing (..)

import Html exposing (div, button, text, table, tr, th, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal


type Msg
    = SignalMsg Signal.Msg
    | SwitchLanguage Language


type Language
    = German
    | English


type alias Model =
    { distantSignal : Signal.Model
    , mainSignal : Signal.Model
    , language : Language
    }


init : Model
init =
    { distantSignal = Signal.distantSignal
    , mainSignal = Signal.mainSignal
    , language = German
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SignalMsg signalmsg ->
            let
                newDistantSignal =
                    Signal.update signalmsg model.distantSignal

                newMainSignal =
                    Signal.update signalmsg model.mainSignal
            in
                { model
                    | distantSignal = newDistantSignal
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
                , th [] [ translate model "Hauptsignal" "Main Signal" ]
                ]
            , tr []
                [ td [] [ Html.map SignalMsg (Signal.view model.distantSignal) ]
                , td [] [ Html.map SignalMsg (Signal.view model.mainSignal) ]
                ]
            , tr []
                [ td [] []
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick (SignalMsg Signal.Danger) ]
                        [ translate model "Halt" "Danger" ]
                    , button [ onClick (SignalMsg Signal.Proceed) ]
                        [ translate model "Fahrt" "Proceed" ]
                    ]
                ]
            ]
        ]


main =
    Html.beginnerProgram { model = init, view = view, update = update }
