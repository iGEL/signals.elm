module SignalModel exposing (..)

import Messages exposing (..)
import Zs3


type ExtraLight
    = Absent
    | Repeater
    | ShortenedBrakePath


type alias StateModel =
    { aspect : Messages.Msg
    , speedLimit : Maybe Int
    , extraLight : ExtraLight
    , hasRa12 : Bool
    , hasZs1 : Bool
    , hasZs7 : Bool
    , zs3 : Zs3.Appearance
    }


type Model
    = DistantSignal StateModel
    | CombinationSignal { mainSignal : StateModel, distantSignal : StateModel }
    | MainSignal StateModel


defaultStateModel : StateModel
defaultStateModel =
    { aspect = Stop
    , speedLimit = Nothing
    , extraLight = Absent
    , hasRa12 = False
    , hasZs1 = False
    , hasZs7 = False
    , zs3 = Zs3.Absent
    }


distantSignal : Model
distantSignal =
    DistantSignal defaultStateModel


signalRepeater : Model
signalRepeater =
    DistantSignal { defaultStateModel | extraLight = Repeater }


combinationSignal : Model
combinationSignal =
    CombinationSignal { mainSignal = defaultStateModel, distantSignal = defaultStateModel }


mainSignal : Model
mainSignal =
    MainSignal defaultStateModel


zs3 : Model -> Zs3.Appearance
zs3 model =
    case model of
        MainSignal state ->
            state.zs3

        CombinationSignal states ->
            states.mainSignal.zs3

        DistantSignal _ ->
            Zs3.Absent


zs3v : Model -> Zs3.Appearance
zs3v model =
    case model of
        MainSignal _ ->
            Zs3.Absent

        CombinationSignal states ->
            states.distantSignal.zs3

        DistantSignal state ->
            state.zs3


isStopMsg : Msg -> Bool
isStopMsg msg =
    (List.member msg [ Stop, StopAndRa12, StopAndZs1, StopAndZs7 ])


isStopState : StateModel -> Bool
isStopState state =
    isStopMsg state.aspect


isStop : Model -> Bool
isStop model =
    case model of
        MainSignal state ->
            isStopState state

        CombinationSignal states ->
            isStopState states.mainSignal

        DistantSignal _ ->
            False


isExpectStop : Model -> Bool
isExpectStop model =
    case model of
        DistantSignal state ->
            isStopState state

        CombinationSignal states ->
            isStopState states.distantSignal

        MainSignal _ ->
            False


isProceedState : StateModel -> Bool
isProceedState state =
    case state.aspect of
        Proceed ->
            True

        _ ->
            False


isProceed : Model -> Bool
isProceed model =
    case model of
        MainSignal state ->
            isProceedState state

        CombinationSignal states ->
            isProceedState states.mainSignal

        DistantSignal _ ->
            False


isExpectProceed : Model -> Bool
isExpectProceed model =
    case model of
        DistantSignal state ->
            isProceedState state

        CombinationSignal states ->
            isProceedState states.distantSignal

        MainSignal _ ->
            False


shortenedBrakePath : Model -> Bool
shortenedBrakePath model =
    case model of
        DistantSignal state ->
            state.extraLight == ShortenedBrakePath

        CombinationSignal states ->
            states.distantSignal.extraLight == ShortenedBrakePath

        MainSignal _ ->
            False


hasRa12 : Model -> Bool
hasRa12 model =
    case model of
        CombinationSignal states ->
            states.mainSignal.hasRa12

        MainSignal state ->
            state.hasRa12

        DistantSignal _ ->
            False


hasZs1 : Model -> Bool
hasZs1 model =
    case model of
        CombinationSignal states ->
            states.mainSignal.hasZs1

        MainSignal state ->
            state.hasZs1

        DistantSignal _ ->
            False


hasZs7 : Model -> Bool
hasZs7 model =
    case model of
        CombinationSignal states ->
            states.mainSignal.hasZs7

        MainSignal state ->
            state.hasZs7

        _ ->
            False


isSpeedLimitState : StateModel -> Bool
isSpeedLimitState state =
    case state.speedLimit of
        Just _ ->
            True

        Nothing ->
            False


hasSpeedLimit : Model -> Bool
hasSpeedLimit model =
    case model of
        CombinationSignal states ->
            isSpeedLimitState states.mainSignal

        MainSignal state ->
            isSpeedLimitState state

        DistantSignal _ ->
            False


hasExpectedSpeedLimit : Model -> Bool
hasExpectedSpeedLimit model =
    case model of
        CombinationSignal states ->
            isSpeedLimitState states.distantSignal

        DistantSignal state ->
            isSpeedLimitState state

        MainSignal _ ->
            False
