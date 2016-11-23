module Messages exposing (..)


type Msg
    = Stop
    | Proceed


type Target
    = ToDistantSignal Msg
    | ToMainSignal Msg
