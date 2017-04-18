module Messages exposing (..)


type alias SpeedLimit =
    Int


type Msg
    = Stop
    | Proceed
    | StopAndZs1
    | StopAndZs7
    | StopAndRa12
    | ResetZs1
    | ToggleHasZs1
    | SetZs3Absent
    | SetZs3Dynamic
    | SetZs3Fixed
    | SetZs3SpeedLimit (Maybe SpeedLimit)
    | SetZs3ForceOff Bool
    | ToggleHasZs7
    | ToggleHasRa12
    | ToggleShortBrakePath


type DisplayMsg
    = Off
    | On String


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
