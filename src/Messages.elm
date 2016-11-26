module Messages exposing (..)


type Msg
    = Stop
    | Proceed
    | ToggleShortBrakePath


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
