module Lamp exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


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


tinyLamp color on x y =
    lamp color False on x y "2"
