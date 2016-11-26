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
        }
    | MainSignal { mainState : Messages.Msg }


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
        }


mainSignal : Model
mainSignal =
    MainSignal { mainState = Stop }


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal representation ->
            case target of
                ToDistantSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            DistantSignal { representation | shortBrakePath = not representation.shortBrakePath }

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

                        _ ->
                            CombinationSignal { representation | distantState = newState }

                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

                        _ ->
                            CombinationSignal { representation | mainState = newState }

        MainSignal representation ->
            case target of
                ToMainSignal newState ->
                    case newState of
                        ToggleShortBrakePath ->
                            model

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


isStop lights =
    lights.mainState == Stop


isProceed lights =
    lights.mainState == Proceed


isExpectStop lights =
    lights.distantState == Stop


isExpectProceed lights =
    lights.distantState == Proceed


viewMainLights signal =
    g []
        [ circle
            [ cx "32"
            , cy "32"
            , r "7.5"
            , class
                (if isStop signal then
                    "red on"
                 else
                    "red off"
                )
            ]
            []
        , circle
            [ cx "32"
            , cy "57.3"
            , r "7.5"
            , class
                (if isProceed signal then
                    "green on"
                 else
                    "green off"
                )
            ]
            []
        ]


viewCombinationLights signal =
    g []
        [ circle
            [ cx "32"
            , cy "32"
            , r "7.5"
            , class
                (if isStop signal then
                    "red on"
                 else
                    "red off"
                )
            ]
            []
        , circle
            [ cx "47.5"
            , cy "57.3"
            , r "7.5"
            , class
                (if (isProceed signal && isExpectStop signal) then
                    "orange on"
                 else
                    "orange off"
                )
            ]
            []
        , circle
            [ cx "16.5"
            , cy "57.3"
            , r "7.5"
            , class
                (if (isProceed signal && isExpectProceed signal) then
                    "green on"
                 else
                    "green off"
                )
            ]
            []
        , if signal.shortBrakePath then
            circle
                [ cx "16.5"
                , cy "14.5"
                , r "3.5"
                , class
                    (if (isProceed signal && isExpectStop signal) then
                        "white on"
                     else
                        "white off"
                    )
                ]
                []
          else
            g [] []
        ]


viewDistantSignal signal =
    g []
        [ circle
            [ cx "47.5"
            , cy "57.3"
            , r "7.5"
            , class
                (if isExpectStop signal then
                    "orange on"
                 else
                    "orange off"
                )
            ]
            []
        , circle
            [ cx "16.5"
            , cy "57.3"
            , r "7.5"
            , class
                (if isExpectProceed signal then
                    "green on"
                 else
                    "green off"
                )
            ]
            []
        , if signal.repeater then
            circle
                [ cx "11.5"
                , cy "98.7"
                , r "3.5"
                , class
                    (if isExpectStop signal then
                        "white on"
                     else
                        "white off"
                    )
                ]
                []
          else
            g [] []
        , if (not signal.repeater && signal.shortBrakePath) then
            circle
                [ cx "16.5"
                , cy "14.5"
                , r "3.5"
                , class
                    (if isExpectStop signal then
                        "white on"
                     else
                        "white off"
                    )
                ]
                []
          else
            g [] []
        ]
