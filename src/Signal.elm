module Signal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)


type Model
    = DistantSignal { distantState : Messages.Msg, repeater : Bool }
    | CombinationSignal { distantState : Messages.Msg, mainState : Messages.Msg }
    | MainSignal { mainState : Messages.Msg }


distantSignal : Model
distantSignal =
    DistantSignal { distantState = Stop, repeater = False }


signalRepeater : Model
signalRepeater =
    DistantSignal { distantState = Stop, repeater = True }


combinationSignal : Model
combinationSignal =
    CombinationSignal
        { distantState = Stop
        , mainState = Stop
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
                    DistantSignal { representation | distantState = newState }

                _ ->
                    model

        CombinationSignal representation ->
            case target of
                ToDistantSignal newState ->
                    CombinationSignal { representation | distantState = newState }

                ToMainSignal newState ->
                    CombinationSignal { representation | mainState = newState }

        MainSignal representation ->
            case target of
                ToMainSignal newState ->
                    MainSignal { representation | mainState = newState }

                _ ->
                    model


view model =
    svg [ version "1.1", viewBox "0 0 70 200", width "200" ]
        [ Svg.style []
            [ text """
                    circle { stroke-width: 0.25; stroke-opacity: 0.85; stroke: #333; }
                    .green { fill:url(#green-gradient) }
                    .red { fill:url(#red-gradient);  }
                    .orange { fill:url(#orange-gradient) }
                    .yellow { fill:url(#yellow-gradient) }
                    .white { fill:url(#white-gradient) }
                    circle.on { fill-opacity: 1; transition: fill-opacity .1s ease-in; transition-delay: .2s }
                    circle.off { fill-opacity: 0; transition: fill-opacity .3s ease-out }
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
        [ circle
            [ cx "32"
            , cy "32"
            , r "7.5"
            , class
                (if signal.mainState == Stop then
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
                (if signal.mainState == Proceed then
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
                (if signal.mainState == Stop then
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
                (if (signal.mainState == Proceed && signal.distantState == Stop) then
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
                (if (signal.mainState == Proceed && signal.distantState == Proceed) then
                    "green on"
                 else
                    "green off"
                )
            ]
            []
        ]


viewDistantSignal signal =
    g []
        [ circle
            [ cx "47.5"
            , cy "57.3"
            , r "7.5"
            , class
                (if signal.distantState == Stop then
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
                (if signal.distantState == Proceed then
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
                    (if signal.distantState == Stop then
                        "white on"
                     else
                        "white off"
                    )
                ]
                []
          else
            g [] []
        ]
