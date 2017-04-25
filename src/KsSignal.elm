module KsSignal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Lamp


type alias Model =
    { topWhiteLight : Lamp.State
    , redLight : Lamp.State
    , greenLight : Lamp.State
    , orangeLight : Lamp.State
    , centerWhiteLight : Lamp.State
    , zs7Lights : Lamp.State
    , bottomWhiteLight : Lamp.State
    }


view : Model -> List (Svg msg)
view model =
    let
        identity n =
            n
    in
        (List.filterMap
            identity
            [ Just (rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] [])
            , Lamp.maybeSmallLamp model.topWhiteLight "white" "16.5" "14.5"
            , Lamp.maybeBigLamp model.redLight "red" "32" "32"
            , Lamp.maybeBigLamp model.greenLight
                "green"
                (if model.orangeLight == Lamp.Absent then
                    "32"
                 else
                    "16.5"
                )
                "57.3"
            , Lamp.maybeBigLamp model.orangeLight "orange" "47.5" "57.3"
            , Lamp.maybeSmallLamp model.centerWhiteLight "white" "32" "81"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "21.5" "81"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "32" "98.7"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "42.5" "81"
            , Lamp.maybeSmallLamp model.bottomWhiteLight "white" "11.5" "98.7"
            ]
        )
