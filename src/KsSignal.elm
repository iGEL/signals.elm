module KsSignal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)


type Model
    = DistantSignal
        { distantState : Messages.Msg
        , repeater : Bool
        , shortBrakePath : Bool
        }
    | CombinationSignal
        { distantState : Messages.Msg
        , mainState : Messages.Msg
        , shortBrakePath : Bool
        , hasZs1 : Bool
        , hasZs7 : Bool
        }
    | MainSignal { mainState : Messages.Msg, hasZs1 : Bool, hasZs7 : Bool }


distantSignal : Model
distantSignal =
    DistantSignal
        { distantState = Stop
        , repeater = False
        , shortBrakePath = False
        }


signalRepeater : Model
signalRepeater =
    DistantSignal
        { distantState = Stop
        , repeater = True
        , shortBrakePath = False
        }


combinationSignal : Model
combinationSignal =
    CombinationSignal
        { distantState = Stop
        , mainState = Stop
        , shortBrakePath = False
        , hasZs1 = False
        , hasZs7 = False
        }


mainSignal : Model
mainSignal =
    MainSignal { mainState = Stop, hasZs1 = False, hasZs7 = False }


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal representation ->
            case target of
                ToDistantSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            DistantSignal { representation | shortBrakePath = not representation.shortBrakePath }

                        ToggleHasZs1 ->
                            model

                        ToggleHasZs7 ->
                            model

                        _ ->
                            DistantSignal { representation | distantState = newState }

                _ ->
                    model

        CombinationSignal representation ->
            case target of
                ToDistantSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            CombinationSignal { representation | shortBrakePath = not representation.shortBrakePath }

                        ToggleHasZs1 ->
                            model

                        ToggleHasZs7 ->
                            model

                        _ ->
                            CombinationSignal { representation | distantState = newState }

                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

                        ToggleHasZs1 ->
                            CombinationSignal { representation | hasZs1 = not representation.hasZs1 }

                        ToggleHasZs7 ->
                            CombinationSignal { representation | hasZs7 = not representation.hasZs7 }

                        _ ->
                            CombinationSignal { representation | mainState = newState }

        MainSignal representation ->
            case target of
                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

                        ToggleHasZs1 ->
                            MainSignal { representation | hasZs1 = not representation.hasZs1 }

                        ToggleHasZs7 ->
                            MainSignal { representation | hasZs7 = not representation.hasZs7 }

                        _ ->
                            MainSignal { representation | mainState = newState }

                _ ->
                    model


view model =
    svg [ version "1.1", viewBox "0 0 70 200", width "200" ]
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
        , g [ transform "translate(3)" ]
            [ rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
            , case model of
                DistantSignal signal ->
                    viewDistantSignal signal

                CombinationSignal signal ->
                    viewCombinationLights signal

                MainSignal signal ->
                    viewMainLights signal
            ]
        ]


viewMainLights signal =
    g []
        [ bigLamp "red" (isStop signal) "32" "32"
        , bigLamp "green" (isProceed signal) "32" "57.3"
        , if signal.hasZs1 then
            smallBlinkingLamp "white" (isZs1 signal) "32" "81"
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


viewCombinationLights signal =
    g []
        [ bigLamp "red" (isStop signal) "32" "32"
        , bigLamp "orange" (isProceed signal && isExpectStop signal) "47.5" "57.3"
        , bigLamp "green" (isProceed signal && isExpectProceed signal) "16.5" "57.3"
        , if signal.hasZs1 then
            smallBlinkingLamp "white" (isZs1 signal) "32" "81"
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
        , if signal.shortBrakePath then
            smallLamp "white" (isProceed signal && isExpectStop signal) "16.5" "14.5"
          else
            g [] []
        ]


viewDistantSignal signal =
    g []
        [ bigLamp "orange" (isExpectStop signal) "47.5" "57.3"
        , bigLamp "green" (isExpectProceed signal) "16.5" "57.3"
        , if signal.repeater then
            smallLamp "white" (isExpectStop signal) "11.5" "98.7"
          else
            g [] []
        , if (not signal.repeater && signal.shortBrakePath) then
            smallLamp "white" (isExpectStop signal) "16.5" "14.5"
          else
            g [] []
        ]


isStop lights =
    lights.mainState
        == Stop
        || lights.mainState
        == StopAndZs1
        || lights.mainState
        == StopAndZs7


isProceed lights =
    lights.mainState == Proceed


isExpectStop lights =
    lights.distantState
        == Stop
        || lights.distantState
        == StopAndZs1
        || lights.distantState
        == StopAndZs7


isExpectProceed lights =
    lights.distantState == Proceed


isZs1 lights =
    lights.mainState == StopAndZs1


isZs7 lights =
    lights.mainState == StopAndZs7


lamp color blinking on x y radius =
    let
        onClass =
            if blinking then
                color ++ " on blinking"
            else
                color ++ " on"

        offClass =
            color ++ " off"
    in
        circle
            [ cx x
            , cy y
            , r radius
            , class
                (if on then
                    onClass
                 else
                    offClass
                )
            ]
            []


bigLamp color on x y =
    bigOptionallyBlinkingLamp color False on x y


smallLamp color on x y =
    smallOptionallyBlinkingLamp color False on x y


bigBlinkingLamp color on x y =
    bigOptionallyBlinkingLamp color True on x y


smallBlinkingLamp color on x y =
    smallOptionallyBlinkingLamp color True on x y


bigOptionallyBlinkingLamp color blinking on x y =
    lamp color blinking on x y "7.5"


smallOptionallyBlinkingLamp color blinking on x y =
    lamp color blinking on x y "3.5"
