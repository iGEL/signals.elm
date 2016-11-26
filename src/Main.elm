module Main exposing (..)

import Html exposing (div, button, label, input, text, table, tr, th, td)
import Html.Attributes exposing (style, type_, checked)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import KsSignal
import Messages exposing (..)


type Msg
    = FirstSignalMsg Messages.Msg
    | SecondSignalMsg Messages.Msg
    | SwitchLanguage Language


type Language
    = German
    | English


type alias Model =
    { distantSignal : KsSignal.Model
    , signalRepeater : KsSignal.Model
    , combinationSignal : KsSignal.Model
    , mainSignal : KsSignal.Model
    , language : Language
    }


init : ( Model, Cmd Msg )
init =
    ( { distantSignal = KsSignal.distantSignal
      , signalRepeater = KsSignal.signalRepeater
      , combinationSignal = KsSignal.combinationSignal
      , mainSignal = KsSignal.mainSignal
      , language = German
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstSignalMsg signalmsg ->
            let
                newDistantSignal =
                    KsSignal.update (ToDistantSignal signalmsg) model.distantSignal

                newSignalRepeater =
                    KsSignal.update (ToDistantSignal signalmsg) model.signalRepeater

                newCombinationSignal =
                    KsSignal.update (ToMainSignal signalmsg) model.combinationSignal
            in
                ( { model
                    | distantSignal = newDistantSignal
                    , signalRepeater = newSignalRepeater
                    , combinationSignal = newCombinationSignal
                  }
                , Cmd.none
                )

        SecondSignalMsg signalmsg ->
            let
                newCombinationSignal =
                    KsSignal.update (ToDistantSignal signalmsg) model.combinationSignal

                newMainSignal =
                    KsSignal.update (ToMainSignal signalmsg) model.mainSignal
            in
                ( { model
                    | combinationSignal = newCombinationSignal
                    , mainSignal = newMainSignal
                  }
                , Cmd.none
                )

        SwitchLanguage language ->
            ( { model | language = language }, Cmd.none )


subscriptions model =
    Sub.batch
        [ Time.every second (\n -> FirstSignalMsg ToggleBlink)
        , Time.every second (\n -> SecondSignalMsg ToggleBlink)
        ]


translate model german english =
    case model.language of
        German ->
            text german

        English ->
            text english


shortBrakePath : KsSignal.Model -> Bool
shortBrakePath model =
    case model of
        KsSignal.DistantSignal state ->
            state.shortBrakePath

        KsSignal.CombinationSignal state ->
            state.shortBrakePath

        _ ->
            False


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
                [ td []
                    [ label []
                        [ input
                            [ type_ "checkbox"
                            , checked (shortBrakePath model.distantSignal)
                            , onClick (FirstSignalMsg ToggleShortBrakePath)
                            ]
                            []
                        , translate model "> 5% verkürzter Bremsweg" "> 5% shortened brake path"
                        ]
                    ]
                , td [] []
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ div []
                        [ button [ onClick (FirstSignalMsg Stop) ]
                            [ translate model "Halt" "Stop" ]
                        , button [ onClick (FirstSignalMsg Proceed) ]
                            [ translate model "Fahrt" "Proceed" ]
                        , button [ onClick (FirstSignalMsg StopAndZs1) ]
                            [ translate model "Halt + Zs 1" "Stop + Zs 1" ]
                        ]
                    , label []
                        [ input
                            [ type_ "checkbox"
                            , checked (shortBrakePath model.combinationSignal)
                            , onClick (SecondSignalMsg ToggleShortBrakePath)
                            ]
                            []
                        , translate model "> 5% verkürzter Bremsweg" "> 5% shortened brake path"
                        ]
                    ]
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick (SecondSignalMsg Stop) ]
                        [ translate model "Halt" "Stop" ]
                    , button [ onClick (SecondSignalMsg Proceed) ]
                        [ translate model "Fahrt" "Proceed" ]
                    , button [ onClick (SecondSignalMsg StopAndZs1) ]
                        [ translate model "Halt + Zs 1" "Stop + Zs 1" ]
                    ]
                ]
            , tr []
                [ td [] [ Html.map FirstSignalMsg (KsSignal.view model.distantSignal) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.signalRepeater) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.combinationSignal) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.mainSignal) ]
                ]
            ]
        ]


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
