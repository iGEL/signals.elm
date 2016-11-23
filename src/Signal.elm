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
        [ rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
        , case model of
            DistantSignal signal ->
                viewDistantSignal signal

            CombinationSignal signal ->
                viewCombinationLights signal

            MainSignal signal ->
                viewMainLights signal
        ]


viewMainLights signal =
    g []
        [ circle
            [ cx "32"
            , cy "32"
            , r "7.5"
            , Svg.Attributes.style
                (if signal.mainState == Stop then
                    "fill:#da012a"
                 else
                    "fill:#111"
                )
            ]
            []
        , circle
            [ cx "32"
            , cy "57.3"
            , r "7.5"
            , Svg.Attributes.style
                (if signal.mainState == Proceed then
                    "fill:#02da53"
                 else
                    "fill:#111"
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
            , Svg.Attributes.style
                (if signal.mainState == Stop then
                    "fill:#da012a"
                 else
                    "fill:#111"
                )
            ]
            []
        , circle
            [ cx "47.5"
            , cy "57.3"
            , r "7.5"
            , Svg.Attributes.style
                (if (signal.mainState == Proceed && signal.distantState == Stop) then
                    "fill:#f08700"
                 else
                    "fill:#111"
                )
            ]
            []
        , circle
            [ cx "16.5"
            , cy "57.3"
            , r "7.5"
            , Svg.Attributes.style
                (if (signal.mainState == Proceed && signal.distantState == Proceed) then
                    "fill:#02da53"
                 else
                    "fill:#111"
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
            , Svg.Attributes.style
                (if signal.distantState == Stop then
                    "fill:#f08700"
                 else
                    "fill:#111"
                )
            ]
            []
        , circle
            [ cx "16.5"
            , cy "57.3"
            , r "7.5"
            , Svg.Attributes.style
                (if signal.distantState == Proceed then
                    "fill:#02da53"
                 else
                    "fill:#111"
                )
            ]
            []
        , circle
            [ cx "11.5"
            , cy "98.7"
            , r "3.5"
            , Svg.Attributes.style
                (if (signal.repeater && signal.distantState == Stop) then
                    "fill:#d8d8d8"
                 else
                    "fill:#111"
                )
            ]
            []
        ]
