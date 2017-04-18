module KsSignal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)
import Zs3
import Lamp exposing (..)


type Model
    = DistantSignal
        { distantState : Messages.Msg
        , repeater : Bool
        , shortBrakePath : Bool
        , zs3v : Zs3.Model
        }
    | CombinationSignal
        { distantState : Messages.Msg
        , mainState : Messages.Msg
        , shortBrakePath : Bool
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs7 : Bool
        , zs3 : Zs3.Model
        , zs3v : Zs3.Model
        }
    | MainSignal
        { mainState : Messages.Msg
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs7 : Bool
        , zs3 : Zs3.Model
        }


zs3Model : Model -> Zs3.Model
zs3Model model =
    case model of
        DistantSignal _ ->
            Zs3.distantSignal

        CombinationSignal state ->
            state.zs3

        MainSignal state ->
            state.zs3


distantSignal : Model
distantSignal =
    DistantSignal
        { distantState = Stop
        , repeater = False
        , shortBrakePath = False
        , zs3v = Zs3.distantSignal
        }


signalRepeater : Model
signalRepeater =
    DistantSignal
        { distantState = Stop
        , repeater = True
        , shortBrakePath = False
        , zs3v = Zs3.distantSignal
        }


combinationSignal : Model
combinationSignal =
    CombinationSignal
        { distantState = Stop
        , mainState = Stop
        , shortBrakePath = False
        , hasRa12 = False
        , hasZs1 = False
        , hasZs7 = False
        , zs3 = Zs3.mainSignal
        , zs3v = Zs3.distantSignal
        }


mainSignal : Model
mainSignal =
    MainSignal
        { mainState = Stop
        , hasRa12 = False
        , hasZs1 = False
        , hasZs7 = False
        , zs3 = Zs3.mainSignal
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

        SetZs3Absent ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3Dynamic ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3Fixed ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3SpeedLimit _ ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        ToggleHasZs7 ->
            signalState

        _ ->
            { signalState
                | distantState = msg
                , zs3v = Zs3.update (SetZs3ForceOff (isStopMsg msg)) signalState.zs3v
            }


updateMainSignal msg signalState =
    case msg of
        ToggleShortBrakePath ->
            signalState

        ToggleHasRa12 ->
            { signalState | hasRa12 = not signalState.hasRa12 }

        ToggleHasZs1 ->
            { signalState | hasZs1 = not signalState.hasZs1 }

        SetZs3Absent ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3Dynamic ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3Fixed ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3SpeedLimit _ ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        ToggleHasZs7 ->
            { signalState | hasZs7 = not signalState.hasZs7 }

        _ ->
            { signalState
                | mainState = msg
                , zs3 = Zs3.update (SetZs3ForceOff (isStopMsg msg)) signalState.zs3
            }


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
        , g []
            [ case model of
                MainSignal signal ->
                    Zs3.view signal.zs3

                CombinationSignal signal ->
                    Zs3.view signal.zs3

                _ ->
                    g [] []
            ]
        , g [ transform "translate(3, 65)" ]
            [ rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
            , case model of
                DistantSignal signal ->
                    viewDistantSignal signal

                CombinationSignal signal ->
                    viewCombinationLights signal

                MainSignal signal ->
                    viewMainLights signal
            ]
        , g [ transform "translate(0, 168)" ]
            [ case model of
                DistantSignal signal ->
                    Zs3.view signal.zs3v

                CombinationSignal signal ->
                    Zs3.view signal.zs3v

                _ ->
                    g [] []
            ]
        ]


viewMainLights signal =
    g []
        [ viewMainAndCombinationLights signal
        , bigLamp "green" (isProceed signal) "32" "57.3"
        , if signal.hasRa12 then
            smallLamp "white" (isRa12 signal) "11.5" "98.7"
          else
            g [] []
        , if signal.hasRa12 || signal.hasZs1 then
            smallOptionallyBlinkingLamp "white" (isZs1 signal) (isRa12 signal || isZs1 signal) "32" "81"
          else
            g [] []
        ]


viewCombinationLights signal =
    g []
        [ viewMainAndCombinationLights signal
        , bigLamp "orange" (isProceed signal && isExpectStop signal) "47.5" "57.3"
        , bigOptionallyBlinkingLamp "green" (isZs3v signal) (isProceed signal && isExpectProceed signal) "16.5" "57.3"
        , if signal.hasRa12 || signal.hasZs1 then
            smallOptionallyBlinkingLamp "white" (isZs1 signal) (isRa12 signal || isZs1 signal) "11.5" "98.7"
          else
            g [] []
        , if signal.hasRa12 then
            smallLamp "white" (isRa12 signal) "32" "81"
          else
            g [] []
        , if signal.shortBrakePath then
            smallLamp "white" (isProceed signal && (isExpectStop signal || isZs3v signal)) "16.5" "14.5"
          else
            g [] []
        ]


viewMainAndCombinationLights signal =
    g []
        [ bigLamp "red" (isStop signal) "32" "32"
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


isStopMsg : Msg -> Bool
isStopMsg msg =
    (List.member msg [ Stop, StopAndRa12, StopAndZs1, StopAndZs7 ])


isStop lights =
    isStopMsg lights.mainState


isProceed lights =
    case lights.mainState of
        Proceed ->
            True

        _ ->
            False


isExpectStop lights =
    isStopMsg lights.distantState


isExpectProceed lights =
    case lights.distantState of
        Proceed ->
            True

        _ ->
            False


isRa12 lights =
    lights.mainState == StopAndRa12


isZs1 lights =
    lights.mainState == StopAndZs1


isZs3v lights =
    case lights.distantState of
        Proceed ->
            Zs3.hasSpeedLimit lights.zs3v

        _ ->
            False


isZs7 lights =
    lights.mainState == StopAndZs7
