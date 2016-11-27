module Main exposing (..)

import Html exposing (div, button, label, input, text, table, tr, th, td)
import Html.Attributes exposing (style, type_, checked, disabled)
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


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\n -> Sub.none)
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
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (shortBrakePath model.combinationSignal)
                                , onClick (SecondSignalMsg ToggleShortBrakePath)
                                ]
                                []
                            , translate model "> 5% verkürzter Bremsweg" "> 5% shortened brake path"
                            ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasRa12 model.combinationSignal)
                                , onClick (FirstSignalMsg ToggleHasRa12)
                                ]
                                []
                            , translate model "Rangierfahrt erlaubt Sh 1/Ra 12" "Shunting movement permitted Sh 1/Ra 12"
                            ]
                        , button
                            [ onClick (FirstSignalMsg StopAndRa12)
                            , disabled (not (hasRa12 model.combinationSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasZs1 model.combinationSignal)
                                , onClick (FirstSignalMsg ToggleHasZs1)
                                ]
                                []
                            , translate model "Ersatzsignal Zs1" "Subsidiary signal Zs1"
                            ]
                        , button
                            [ onClick (FirstSignalMsg StopAndZs1)
                            , disabled (not (hasZs1 model.combinationSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasZs7 model.combinationSignal)
                                , onClick (FirstSignalMsg ToggleHasZs7)
                                ]
                                []
                            , translate model "Vorsichtsignal Zs7" "Caution signal Zs7"
                            ]
                        , button
                            [ onClick (FirstSignalMsg StopAndZs7)
                            , disabled (not (hasZs7 model.combinationSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
                    ]
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ div []
                        [ button [ onClick (SecondSignalMsg Stop) ]
                            [ translate model "Halt" "Stop" ]
                        , button [ onClick (SecondSignalMsg Proceed) ]
                            [ translate model "Fahrt" "Proceed" ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasRa12 model.mainSignal)
                                , onClick (SecondSignalMsg ToggleHasRa12)
                                ]
                                []
                            , translate model "Rangierfahrt erlaubt Sh 1/Ra 12" "Shunting movement permitted Sh 1/Ra 12"
                            ]
                        , button
                            [ onClick (SecondSignalMsg StopAndRa12)
                            , disabled (not (hasRa12 model.mainSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasZs1 model.mainSignal)
                                , onClick (SecondSignalMsg ToggleHasZs1)
                                ]
                                []
                            , translate model "Ersatzsignal Zs1" "Subsidiary signal Zs1"
                            ]
                        , button
                            [ onClick (SecondSignalMsg StopAndZs1)
                            , disabled (not (hasZs1 model.mainSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
                    , div []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , checked (hasZs7 model.mainSignal)
                                , onClick (SecondSignalMsg ToggleHasZs7)
                                ]
                                []
                            , translate model "Vorsichtsignal Zs7" "Caution signal Zs7"
                            ]
                        , button
                            [ onClick (SecondSignalMsg StopAndZs7)
                            , disabled (not (hasZs7 model.mainSignal))
                            ]
                            [ translate model "Aktiv" "Active" ]
                        ]
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


shortBrakePath : KsSignal.Model -> Bool
shortBrakePath model =
    case model of
        KsSignal.DistantSignal state ->
            state.shortBrakePath

        KsSignal.CombinationSignal state ->
            state.shortBrakePath

        _ ->
            False


hasRa12 : KsSignal.Model -> Bool
hasRa12 model =
    case model of
        KsSignal.CombinationSignal state ->
            state.hasRa12

        KsSignal.MainSignal state ->
            state.hasRa12

        _ ->
            False


hasZs1 : KsSignal.Model -> Bool
hasZs1 model =
    case model of
        KsSignal.CombinationSignal state ->
            state.hasZs1

        KsSignal.MainSignal state ->
            state.hasZs1

        _ ->
            False


hasZs7 : KsSignal.Model -> Bool
hasZs7 model =
    case model of
        KsSignal.CombinationSignal state ->
            state.hasZs7

        KsSignal.MainSignal state ->
            state.hasZs7

        _ ->
            False
