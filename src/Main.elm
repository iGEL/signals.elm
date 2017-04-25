module Main exposing (..)

import Html exposing (div, button, label, input, text, table, tr, th, td, select, option)
import Html.Attributes exposing (style, type_, checked, disabled, value)
import Html.Events exposing (onClick)
import Signal
import Zs3
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
    { distantSignal : Signal.Model
    , signalRepeater : Signal.Model
    , combinationSignal : Signal.Model
    , mainSignal : Signal.Model
    , language : Language
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\n -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    ( { distantSignal = Signal.distantSignal
      , signalRepeater = Signal.signalRepeater
      , combinationSignal = Signal.combinationSignal
      , mainSignal = Signal.mainSignal
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
                    Signal.update (ToDistantSignal signalmsg) model.distantSignal

                newSignalRepeater =
                    Signal.update (ToDistantSignal signalmsg) model.signalRepeater

                newCombinationSignal =
                    Signal.update (ToMainSignal signalmsg) model.combinationSignal
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
                    Signal.update (ToDistantSignal signalmsg) model.combinationSignal

                newMainSignal =
                    Signal.update (ToMainSignal signalmsg) model.mainSignal
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
                [ td [] [ Html.map FirstSignalMsg (Signal.view model.distantSignal) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.signalRepeater) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.combinationSignal) ]
                , td [] [ Html.map FirstSignalMsg (Signal.view model.mainSignal) ]
                ]
            ]
        ]


distantSignalOptions : (Messages.Msg -> b) -> Signal.Model -> { a | language : Language } -> Html.Html b
distantSignalOptions targetSignal signal model =
    optionWithoutButton
        { toggleActive = shortBrakePath signal
        , toggleMsg = targetSignal ToggleShortBrakePath
        , toggleLabel = translate model "> 5% verkürzter Bremsweg" "> 5% shortened brake path"
        }


mainSignalOptions : (Messages.Msg -> msg) -> Signal.Model -> { a | language : Language } -> Html.Html msg
mainSignalOptions targetSignal signal model =
    div []
        [ button [ onClick (targetSignal Stop) ]
            [ translate model "Halt" "Stop" ]
        , button [ onClick (targetSignal Proceed) ]
            [ translate model "Fahrt" "Proceed" ]
        , div []
            [ label []
                [ input
                    [ type_ "radio"
                    , checked (Zs3.isAbsent (Signal.zs3Model signal))
                    , onClick (targetSignal SetZs3Absent)
                    ]
                    []
                , translate model "Kein Zs3" "No Zs3"
                ]
            , label []
                [ input
                    [ type_ "radio"
                    , checked (Zs3.isDynamic (Signal.zs3Model signal))
                    , onClick (targetSignal SetZs3Dynamic)
                    ]
                    []
                , translate model "Dynamisches Zs3" "Dynamic Zs3"
                ]
            , label []
                [ input
                    [ type_ "radio"
                    , checked (Zs3.isFixed (Signal.zs3Model signal))
                    , onClick (targetSignal SetZs3Fixed)
                    ]
                    []
                , translate model "Zs3-Schild" "Zs3 sign"
                ]
            , speedDropdown
                { active = (hasZs3 signal)
                , includeUnlimited = Zs3.isDynamic (Signal.zs3Model signal)
                , actionMsg = targetSignal
                }
            ]
        , optionWithButton
            { toggleActive = hasRa12 signal
            , toggleMsg = targetSignal ToggleHasRa12
            , toggleLabel = translate model "Rangierfahrt erlaubt Sh 1/Ra 12" "Shunting permitted Sh 1/Ra 12"
            , actionMsg = targetSignal StopAndRa12
            , actionLabel = translate model "Aktiv" "Active"
            }
        , optionWithButton
            { toggleActive = hasZs1 signal
            , toggleMsg = targetSignal ToggleHasZs1
            , toggleLabel = translate model "Ersatzsignal Zs1" "Subsidiary signal Zs1"
            , actionMsg = targetSignal StopAndZs1
            , actionLabel = translate model "Aktiv" "Active"
            }
        , optionWithButton
            { toggleActive = hasZs7 signal
            , toggleMsg = targetSignal ToggleHasZs7
            , toggleLabel = translate model "Vorsichtsignal Zs7" "Caution signal Zs7"
            , actionMsg = targetSignal StopAndZs7
            , actionLabel = translate model "Aktiv" "Active"
            }
        ]


translate : { a | language : Language } -> String -> String -> Html.Html msg
translate model german english =
    case model.language of
        German ->
            text german

        English ->
            text english


optionWithoutButton : { b | toggleActive : Bool, toggleLabel : Html.Html a, toggleMsg : a } -> Html.Html a
optionWithoutButton options =
    label []
        [ input
            [ type_ "checkbox"
            , checked options.toggleActive
            , onClick options.toggleMsg
            ]
            []
        , options.toggleLabel
        ]


optionWithButton : { a | actionLabel : Html.Html msg, actionMsg : msg, toggleActive : Bool, toggleLabel : Html.Html msg, toggleMsg : msg } -> Html.Html msg
optionWithButton options =
    div []
        [ optionWithoutButton options
        , button
            [ onClick options.actionMsg
            , disabled (not options.toggleActive)
            ]
            [ options.actionLabel ]
        ]


speedDropdown : { a | actionMsg : Messages.Msg -> msg, active : Bool, includeUnlimited : Bool } -> Html.Html msg
speedDropdown options =
    let
        speeds =
            if options.includeUnlimited then
                List.append (List.range 1 15) [ 0 ]
            else
                List.range 1 15
    in
        select
            [ onSelectChange
                (\selectedValue ->
                    let
                        selectedValueInt =
                            selectedValue
                                |> String.toInt
                                |> Result.withDefault 0

                        selectedSpeed =
                            if selectedValueInt == 0 then
                                Nothing
                            else
                                Just selectedValueInt
                    in
                        selectedSpeed
                            |> SetZs3SpeedLimit
                            |> options.actionMsg
                )
            , disabled (not options.active)
            ]
            (List.map
                (\speed ->
                    option
                        [ value (toString speed) ]
                        [ text
                            (if speed == 0 then
                                "∞"
                             else
                                ((toString (speed * 10)) ++ " km/h")
                            )
                        ]
                )
                speeds
            )


shortBrakePath : Signal.Model -> Bool
shortBrakePath model =
    case model of
        Signal.DistantSignal state ->
            state.shortBrakePath

        Signal.CombinationSignal state ->
            state.shortBrakePath

        _ ->
            False


hasRa12 : Signal.Model -> Bool
hasRa12 model =
    case model of
        Signal.CombinationSignal state ->
            state.hasRa12

        Signal.MainSignal state ->
            state.hasRa12

        _ ->
            False


hasZs1 : Signal.Model -> Bool
hasZs1 model =
    case model of
        Signal.CombinationSignal state ->
            state.hasZs1

        Signal.MainSignal state ->
            state.hasZs1

        _ ->
            False


hasZs3 : Signal.Model -> Bool
hasZs3 model =
    case model of
        Signal.CombinationSignal state ->
            not (state.zs3.appearance == Zs3.Absent)

        Signal.MainSignal state ->
            not (state.zs3.appearance == Zs3.Absent)

        _ ->
            False


hasZs7 : Signal.Model -> Bool
hasZs7 model =
    case model of
        Signal.CombinationSignal state ->
            state.hasZs7

        Signal.MainSignal state ->
            state.hasZs7

        _ ->
            False
