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
        , blink : Bool
        }
    | MainSignal { mainState : Messages.Msg, blink : Bool }


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
        , blink = False
        }


mainSignal : Model
mainSignal =
    MainSignal { mainState = Stop, blink = False }


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal representation ->
            case target of
                ToDistantSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            DistantSignal { representation | shortBrakePath = not representation.shortBrakePath }

                        ToggleBlink ->
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

                        ToggleBlink ->
                            model

                        _ ->
                            CombinationSignal { representation | distantState = newState }

                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

                        ToggleBlink ->
                            CombinationSignal { representation | blink = not representation.blink }

                        _ ->
                            CombinationSignal { representation | mainState = newState }

        MainSignal representation ->
            case target of
                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

                        ToggleBlink ->
                            MainSignal { representation | blink = not representation.blink }

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
        , smallLamp "white" (isZs1 signal && signal.blink) "32" "81"
        ]


viewCombinationLights signal =
    g []
        [ bigLamp "red" (isStop signal) "32" "32"
        , bigLamp "orange" (isProceed signal && isExpectStop signal) "47.5" "57.3"
        , bigLamp "green" (isProceed signal && isExpectProceed signal) "16.5" "57.3"
        , smallLamp "white" (isZs1 signal && signal.blink) "32" "81"
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
    lights.mainState == Stop || lights.mainState == StopAndZs1


isProceed lights =
    lights.mainState == Proceed


isExpectStop lights =
    lights.distantState == Stop || lights.distantState == StopAndZs1


isExpectProceed lights =
    lights.distantState == Proceed


isZs1 lights =
    lights.mainState == StopAndZs1


lamp color on x y radius =
    let
        onClass =
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
    lamp color on x y "7.5"


smallLamp color on x y =
    lamp color on x y "3.5"
