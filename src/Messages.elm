module Messages exposing (..)

import Time exposing (Time)


type Msg
    = Stop
    | Proceed
    | StopAndZs1
    | ToggleShortBrakePath
    | ToggleBlink


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
