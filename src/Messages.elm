module Messages exposing (..)

import Time exposing (Time)


type Msg
    = Stop
    | Proceed
    | StopAndZs1
    | ProceedWithSpeedLimit DisplayMsg
    | StopAndZs7
    | StopAndRa12
    | ResetZs1
    | ToggleHasZs1
    | ToggleHasZs3
    | ToggleHasZs7
    | ToggleHasRa12
    | ToggleShortBrakePath


type DisplayMsg
    = Off
    | On String


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
