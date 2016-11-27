module Messages exposing (..)

import Time exposing (Time)


type Msg
    = Stop
    | Proceed
    | StopAndZs1
    | StopAndZs7
    | ResetZs1
    | ToggleHasZs1
    | ToggleHasZs7
    | ToggleShortBrakePath


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
