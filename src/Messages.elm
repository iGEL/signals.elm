module Messages exposing (..)

import Time exposing (Time)


type Msg
    = Stop
    | Proceed
    | StopAndZs1
    | StopAndZs7
    | StopAndRa12
    | ResetZs1
    | ToggleHasZs1
    | ToggleHasZs7
    | ToggleHasRa12
    | ToggleShortBrakePath


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
