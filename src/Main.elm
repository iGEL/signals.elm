module Main exposing (..)

import Html exposing (div, button, label, input, text, table, tr, th, td, select, option)
import Html.Attributes exposing (style, type_, checked, disabled, value)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import KsSignal
import Messages exposing (..)
import SelectChange exposing (..)


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
                    [ distantSignalOptions FirstSignalMsg model.distantSignal model
                    ]
                , td [] []
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ mainSignalOptions FirstSignalMsg model.combinationSignal model
                    , distantSignalOptions SecondSignalMsg model.combinationSignal model
                    ]
                , td [ style [ ( "text-align", "center" ) ] ]
                    [ mainSignalOptions SecondSignalMsg model.mainSignal model ]
                ]
            , tr []
                [ td [] [ Html.map FirstSignalMsg (KsSignal.view model.distantSignal) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.signalRepeater) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.combinationSignal) ]
                , td [] [ Html.map FirstSignalMsg (KsSignal.view model.mainSignal) ]
                ]
            ]
        ]


distantSignalOptions targetSignal signal model =
    optionWithoutButton
        { toggle_active = shortBrakePath signal
        , toggle_msg = targetSignal ToggleShortBrakePath
        , toggle_label = translate model "> 5% verkürzter Bremsweg" "> 5% shortened brake path"
        }


mainSignalOptions targetSignal signal model =
    div []
        [ button [ onClick (targetSignal Stop) ]
            [ translate model "Halt" "Stop" ]
        , button [ onClick (targetSignal Proceed) ]
            [ translate model "Fahrt" "Proceed" ]
        , div []
            [ optionWithoutButton
                { toggle_active = hasZs3 signal
                , toggle_msg = targetSignal ToggleHasZs3
                , toggle_label = translate model "Geschwindigkeitsanzeiger Zs3" "Speed display Zs3"
                }
            , speedDropdown
                { active = (hasZs3 signal)
                , action_msg = targetSignal
                }
            ]
        , optionWithButton
            { toggle_active = hasRa12 signal
            , toggle_msg = targetSignal ToggleHasRa12
            , toggle_label = translate model "Rangierfahrt erlaubt Sh 1/Ra 12" "Shunting permitted Sh 1/Ra 12"
            , action_msg = targetSignal StopAndRa12
            , action_label = translate model "Aktiv" "Active"
            }
        , optionWithButton
            { toggle_active = hasZs1 signal
            , toggle_msg = targetSignal ToggleHasZs1
            , toggle_label = translate model "Ersatzsignal Zs1" "Subsidiary signal Zs1"
            , action_msg = targetSignal StopAndZs1
            , action_label = translate model "Aktiv" "Active"
            }
        , optionWithButton
            { toggle_active = hasZs7 signal
            , toggle_msg = targetSignal ToggleHasZs7
            , toggle_label = translate model "Vorsichtsignal Zs7" "Caution signal Zs7"
            , action_msg = targetSignal StopAndZs7
            , action_label = translate model "Aktiv" "Active"
            }
        ]


translate model german english =
    case model.language of
        German ->
            text german

        English ->
            text english


optionWithoutButton options =
    label []
        [ input
            [ type_ "checkbox"
            , checked options.toggle_active
            , onClick options.toggle_msg
            ]
            []
        , options.toggle_label
        ]


optionWithButton options =
    div []
        [ optionWithoutButton options
        , button
            [ onClick options.action_msg
            , disabled (not options.toggle_active)
            ]
            [ options.action_label ]
        ]


speedDropdown options =
    select
        [ onSelectChange
            (\selectedSpeed ->
                selectedSpeed
                    |> Messages.On
                    |> ProceedWithSpeedLimit
                    |> options.action_msg
            )
        , disabled (not options.active)
        ]
        (List.map
            (\speed ->
                option
                    [ value (toString (speed * 10)) ]
                    [ text ((toString (speed * 10)) ++ " km/h") ]
            )
            (List.range 1 16)
        )


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


hasZs3 : KsSignal.Model -> Bool
hasZs3 model =
    case model of
        KsSignal.CombinationSignal state ->
            state.hasZs3

        KsSignal.MainSignal state ->
            state.hasZs3

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
