module Lamp exposing (State(..), maybeBigLamp, maybeSmallLamp, tinyLamp)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type State
    = Absent
    | Off
    | Blinking
    | On


lamp : String -> Bool -> Bool -> String -> String -> String -> Svg msg
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


maybeBigLamp : State -> String -> String -> String -> Maybe (Svg msg)
maybeBigLamp state color x y =
    case state of
        Absent ->
            Nothing

        Off ->
            Just (bigLamp color False x y)

        Blinking ->
            Just (bigBlinkingLamp color True x y)

        On ->
            Just (bigLamp color True x y)


maybeSmallLamp : State -> String -> String -> String -> Maybe (Svg msg)
maybeSmallLamp state color x y =
    case state of
        Absent ->
            Nothing

        Off ->
            Just (smallLamp color False x y)

        Blinking ->
            Just (smallBlinkingLamp color True x y)

        On ->
            Just (smallLamp color True x y)
