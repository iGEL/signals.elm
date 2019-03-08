module Messages exposing (Aspect(..), DisplayMsg(..), Msg(..), Target(..))


type alias SpeedLimit =
    Int


type Aspect
    = Stop
    | Proceed
    | StopAndZs1
    | StopAndZs7
    | StopAndRa12


type Msg
    = SetAspect Aspect
    | SetSpeedLimit (Maybe SpeedLimit)
    | SetZs3Absent
    | SetZs3Dynamic
    | SetZs3Fixed
    | ToggleHasRa12
    | ToggleHasZs1
    | ToggleHasZs7
    | ToggleShortBrakePath
    | ToggleSlowSpeedLight Int


type DisplayMsg
    = Off
    | On String


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
