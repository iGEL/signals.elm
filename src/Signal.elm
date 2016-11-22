module Signal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = Danger
    | Proceed


type Model
    = DistantSignal { yellow : Bool, green : Bool }
    | MainSignal { red : Bool, green : Bool }


distantSignal =
    DistantSignal { yellow = True, green = False }


mainSignal =
    MainSignal { red = True, green = False }


update msg model =
    case model of
        DistantSignal lights ->
            case msg of
                Danger ->
                    DistantSignal { lights | green = False, yellow = True }

                Proceed ->
                    DistantSignal { lights | green = True, yellow = False }

        MainSignal lights ->
            case msg of
                Danger ->
                    MainSignal { lights | green = False, red = True }

                Proceed ->
                    MainSignal { lights | green = True, red = False }


view model =
    svg [ version "1.1", viewBox "0 0 70 200", width "200" ]
        [ rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
        , case model of
            DistantSignal lights ->
                viewDistantSignal lights

            MainSignal lights ->
                viewMainLights lights
        ]


viewMainLights lights =
    g []
        [ circle
            [ cx "32"
            , cy "32"
            , r "7.5"
            , Svg.Attributes.style
                (if lights.red then
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
                (if lights.green then
                    "fill:#02da53"
                 else
                    "fill:#111"
                )
            ]
            []
        ]


viewDistantSignal lights =
    g []
        [ circle
            [ cx "47.5"
            , cy "57.3"
            , r "7.5"
            , Svg.Attributes.style
                (if lights.yellow then
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
                (if lights.green then
                    "fill:#02da53"
                 else
                    "fill:#111"
                )
            ]
            []
        ]
