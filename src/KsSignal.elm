module KsSignal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)
import Display
import Lamp exposing (..)


type Model
    = DistantSignal
        { distantState : Messages.Msg
        , repeater : Bool
        , shortBrakePath : Bool
        , hasZs3v : Bool
        }
    | CombinationSignal
        { distantState : Messages.Msg
        , mainState : Messages.Msg
        , shortBrakePath : Bool
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs3 : Bool
        , hasZs3v : Bool
        , hasZs7 : Bool
        }
    | MainSignal
        { mainState : Messages.Msg
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs3 : Bool
        , hasZs7 : Bool
        }


distantSignal : Model
distantSignal =
    DistantSignal
        { distantState = Stop
        , repeater = False
        , shortBrakePath = False
        , hasZs3v = False
        }


signalRepeater : Model
signalRepeater =
    DistantSignal
        { distantState = Stop
        , repeater = True
        , shortBrakePath = False
        , hasZs3v = False
        }


combinationSignal : Model
combinationSignal =
    CombinationSignal
        { distantState = Stop
        , mainState = Stop
        , shortBrakePath = False
        , hasRa12 = False
        , hasZs1 = False
        , hasZs3 = False
        , hasZs3v = False
        , hasZs7 = False
        }


mainSignal : Model
mainSignal =
    MainSignal
        { mainState = Stop
        , hasRa12 = False
        , hasZs1 = False
        , hasZs3 = False
        , hasZs7 = False
        }


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal signalState ->
            case target of
                ToDistantSignal msg ->
                    DistantSignal (updateDistantSignal msg signalState)

                _ ->
                    model

        CombinationSignal signalState ->
            case target of
                ToDistantSignal msg ->
                    CombinationSignal (updateDistantSignal msg signalState)

                ToMainSignal msg ->
                    CombinationSignal (updateMainSignal msg signalState)

        MainSignal signalState ->
            case target of
                ToMainSignal msg ->
                    MainSignal (updateMainSignal msg signalState)

                _ ->
                    model


updateDistantSignal msg signalState =
    case msg of
        ToggleShortBrakePath ->
            { signalState | shortBrakePath = not signalState.shortBrakePath }

        ToggleHasRa12 ->
            signalState

        ToggleHasZs1 ->
            signalState

        ToggleHasZs3 ->
            { signalState | hasZs3v = not signalState.hasZs3v }

        ToggleHasZs7 ->
            signalState

        _ ->
            { signalState | distantState = msg }


updateMainSignal msg signalState =
    case msg of
        ToggleShortBrakePath ->
            signalState

        ToggleHasRa12 ->
            { signalState | hasRa12 = not signalState.hasRa12 }

        ToggleHasZs1 ->
            { signalState | hasZs1 = not signalState.hasZs1 }

        ToggleHasZs3 ->
            { signalState | hasZs3 = not signalState.hasZs3 }

        ToggleHasZs7 ->
            { signalState | hasZs7 = not signalState.hasZs7 }

        _ ->
            { signalState | mainState = msg }


view model =
    svg [ version "1.1", viewBox "0 0 70 300", width "200" ]
        [ Svg.style []
            [ text """
                    circle { stroke-width: 0.4; stroke-opacity: 0.85; stroke: #333; }
                    .green { fill:url(#green-gradient) }
                    .green.on { filter: drop-shadow(0 0 5px #00bd4a) }
                    .red { fill:url(#red-gradient);  }
                    .red.on { filter: drop-shadow(0 0 5px #da012a) }
                    .orange { fill:url(#orange-gradient) }
                    .orange.on { filter: drop-shadow(0 0 5px #fc8e00) }
                    .yellow { fill:url(#yellow-gradient) }
                    .yellow.on { filter: drop-shadow(0 0 5px #fac412) }
                    .white { fill:url(#white-gradient) }
                    .white.on { filter: drop-shadow(0 0 5px #ebe6d8) }
                    circle.on { fill-opacity: 1; transition: fill-opacity .05s ease-in; transition-delay: .1s }
                    circle.off { fill-opacity: 0; transition: fill-opacity .2s ease-out }
                    @keyframes blinking {
                        0% { fill-opacity: 1 }
                        37.5% { fill-opacity: 1 }
                        40% { fill-opacity: 0 }
                        90% { fill-opacity: 0 }
                        100% { fill-opacity: 1 }

                    }
                    circle.blinking { animation: blinking 2s ease infinite }
                    """
            ]
        , defs []
            [ radialGradient [ id "green-gradient" ]
                [ stop [ stopColor "#33ff6d", offset "0.05" ] []
                , stop [ stopColor "#00bd4a", offset "0.9" ] []
                ]
            , radialGradient [ id "red-gradient" ]
                [ stop [ stopColor "#ff3763", offset "0.05" ] []
                , stop [ stopColor "#da012a", offset "0.9" ] []
                ]
            , radialGradient [ id "orange-gradient" ]
                [ stop [ stopColor "#ffc955", offset "0.05" ] []
                , stop [ stopColor "#fc8e00", offset "0.9" ] []
                ]
            , radialGradient [ id "yellow-gradient" ]
                [ stop [ stopColor "#ffe060", offset "0.05" ] []
                , stop [ stopColor "#fac412", offset "0.9" ] []
                ]
            , radialGradient [ id "white-gradient" ]
                [ stop [ stopColor "#fffaef", offset "0.05" ] []
                , stop [ stopColor "#ebe6d8", offset "0.9" ] []
                ]
            ]
        , g [ transform "translate(10, 0)" ]
            [ case model of
                MainSignal signal ->
                    viewZs3
                        { present = signal.hasZs3
                        , forceOffIf = False
                        , color = "white"
                        , state = signal.mainState
                        }

                CombinationSignal signal ->
                    viewZs3
                        { present = signal.hasZs3
                        , forceOffIf = False
                        , color = "white"
                        , state = signal.mainState
                        }

                _ ->
                    g [] []
            ]
        , g [ transform "translate(3, 55)" ]
            [ rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
            , case model of
                DistantSignal signal ->
                    viewDistantSignal signal

                CombinationSignal signal ->
                    viewCombinationLights signal

                MainSignal signal ->
                    viewMainLights signal
            ]
        , g [ transform "translate(10, 180)" ]
            [ case model of
                DistantSignal signal ->
                    viewZs3
                        { present = signal.hasZs3v
                        , forceOffIf = False
                        , color = "orange"
                        , state = signal.distantState
                        }

                CombinationSignal signal ->
                    viewZs3
                        { present = signal.hasZs3v
                        , forceOffIf = isStop signal
                        , color = "orange"
                        , state = signal.distantState
                        }

                _ ->
                    g [] []
            ]
        ]


viewZs3 options =
    if options.present then
        if not options.forceOffIf then
            case options.state of
                ProceedWithSpeedLimit speed ->
                    Display.view options.color speed

                _ ->
                    Display.view options.color Messages.Off
        else
            Display.view options.color Messages.Off
    else
        g [] []


viewMainLights signal =
    g []
        [ viewMainAndCombinationLights signal
        , bigLamp "green" (isProceed signal) "32" "57.3"
        ]


viewCombinationLights signal =
    g []
        [ viewMainAndCombinationLights signal
        , bigLamp "orange" (isProceed signal && isExpectStop signal) "47.5" "57.3"
        , bigOptionallyBlinkingLamp "green" (isZs3v signal) (isProceed signal && isExpectProceed signal) "16.5" "57.3"
        , if signal.shortBrakePath then
            smallLamp "white" (isProceed signal && (isExpectStop signal || isZs3v signal)) "16.5" "14.5"
          else
            g [] []
        ]


viewMainAndCombinationLights signal =
    g []
        [ bigLamp "red" (isStop signal) "32" "32"
        , if signal.hasRa12 then
            smallLamp "white" (isRa12 signal) "11.5" "98.7"
          else
            g [] []
        , if signal.hasRa12 || signal.hasZs1 then
            smallOptionallyBlinkingLamp "white" (isZs1 signal) (isRa12 signal || isZs1 signal) "32" "81"
          else
            g [] []
        , if signal.hasZs7 then
            g []
                [ smallLamp "yellow" (isZs7 signal) "21.5" "81"
                , smallLamp "yellow" (isZs7 signal) "32" "98.7"
                , smallLamp "yellow" (isZs7 signal) "42.5" "81"
                ]
          else
            g [] []
        ]


viewDistantSignal signal =
    g []
        [ bigLamp "orange" (isExpectStop signal) "47.5" "57.3"
        , bigOptionallyBlinkingLamp "green" (isZs3v signal) (isExpectProceed signal) "16.5" "57.3"
        , if signal.repeater then
            smallLamp "white" (isExpectStop signal || isZs3v signal) "11.5" "98.7"
          else
            g [] []
        , if (not signal.repeater && signal.shortBrakePath) then
            smallLamp "white" (isExpectStop signal || isZs3v signal) "16.5" "14.5"
          else
            g [] []
        ]


isStop lights =
    lights.mainState
        == Stop
        || lights.mainState
        == StopAndRa12
        || lights.mainState
        == StopAndZs1
        || lights.mainState
        == StopAndZs7


isProceed lights =
    case lights.mainState of
        Proceed ->
            True

        ProceedWithSpeedLimit _ ->
            True

        _ ->
            False


isExpectStop lights =
    lights.distantState
        == Stop
        || lights.distantState
        == StopAndRa12
        || lights.distantState
        == StopAndZs1
        || lights.distantState
        == StopAndZs7


isExpectProceed lights =
    case lights.distantState of
        Proceed ->
            True

        ProceedWithSpeedLimit _ ->
            True

        _ ->
            False


isRa12 lights =
    lights.mainState == StopAndRa12


isZs1 lights =
    lights.mainState == StopAndZs1


isZs3v lights =
    case lights.distantState of
        ProceedWithSpeedLimit _ ->
            lights.hasZs3v

        _ ->
            False


isZs7 lights =
    lights.mainState == StopAndZs7
