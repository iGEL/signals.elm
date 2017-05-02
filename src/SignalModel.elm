module SignalModel exposing (..)

import Messages exposing (..)
import Zs3


type Model
    = DistantSignal
        { distantState : Messages.Msg
        , repeater : Bool
        , shortBrakePath : Bool
        , zs3v : Zs3.Model
        }
    | CombinationSignal
        { distantState : Messages.Msg
        , mainState : Messages.Msg
        , shortBrakePath : Bool
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs7 : Bool
        , zs3 : Zs3.Model
        , zs3v : Zs3.Model
        }
    | MainSignal
        { mainState : Messages.Msg
        , hasRa12 : Bool
        , hasZs1 : Bool
        , hasZs7 : Bool
        , zs3 : Zs3.Model
        }


distantSignal : Model
distantSignal =
    DistantSignal
        { distantState = Stop
        , repeater = False
        , shortBrakePath = False
        , zs3v = Zs3.distantSignal
        }


signalRepeater : Model
signalRepeater =
    DistantSignal
        { distantState = Stop
        , repeater = True
        , shortBrakePath = False
        , zs3v = Zs3.distantSignal
        }


combinationSignal : Model
combinationSignal =
    CombinationSignal
        { distantState = Stop
        , mainState = Stop
        , shortBrakePath = False
        , hasRa12 = False
        , hasZs1 = False
        , hasZs7 = False
        , zs3 = Zs3.mainSignal
        , zs3v = Zs3.distantSignal
        }


mainSignal : Model
mainSignal =
    MainSignal
        { mainState = Stop
        , hasRa12 = False
        , hasZs1 = False
        , hasZs7 = False
        , zs3 = Zs3.mainSignal
        }


zs3Model : Model -> Zs3.Model
zs3Model model =
    case model of
        DistantSignal _ ->
            Zs3.distantSignal

        CombinationSignal state ->
            state.zs3

        MainSignal state ->
            state.zs3


isStopMsg : Msg -> Bool
isStopMsg msg =
    (List.member msg [ Stop, StopAndRa12, StopAndZs1, StopAndZs7 ])


isStop : { a | mainState : Msg } -> Bool
isStop lights =
    isStopMsg lights.mainState


isProceed : { a | mainState : Msg } -> Bool
isProceed lights =
    case lights.mainState of
        Proceed ->
            True

        _ ->
            False


isExpectStop : { a | distantState : Msg } -> Bool
isExpectStop lights =
    isStopMsg lights.distantState


isExpectProceed : { a | distantState : Msg } -> Bool
isExpectProceed lights =
    case lights.distantState of
        Proceed ->
            True

        _ ->
            False


shortBrakePath : Model -> Bool
shortBrakePath model =
    case model of
        DistantSignal state ->
            state.shortBrakePath

        CombinationSignal state ->
            state.shortBrakePath

        _ ->
            False


hasRa12 : Model -> Bool
hasRa12 model =
    case model of
        CombinationSignal state ->
            state.hasRa12

        MainSignal state ->
            state.hasRa12

        _ ->
            False


hasZs1 : Model -> Bool
hasZs1 model =
    case model of
        CombinationSignal state ->
            state.hasZs1

        MainSignal state ->
            state.hasZs1

        _ ->
            False


hasZs7 : Model -> Bool
hasZs7 model =
    case model of
        CombinationSignal state ->
            state.hasZs7

        MainSignal state ->
            state.hasZs7

        _ ->
            False
