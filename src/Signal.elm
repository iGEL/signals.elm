module Signal exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)
import Zs3
import Lamp exposing (..)
import KsSignal


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


zs3Model : Model -> Zs3.Model
zs3Model model =
    case model of
        DistantSignal _ ->
            Zs3.distantSignal

        CombinationSignal state ->
            state.zs3

        MainSignal state ->
            state.zs3


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


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal signalState ->
            case target of
                ToDistantSignal msg ->
                    DistantSignal (updateDistantSignal msg signalState)

                _ ->
                    model

        CombinationSignal signalState ->
            case target of
                ToDistantSignal msg ->
                    CombinationSignal (updateDistantSignal msg signalState)

                ToMainSignal msg ->
                    CombinationSignal (updateMainSignal msg signalState)

        MainSignal signalState ->
            case target of
                ToMainSignal msg ->
                    MainSignal (updateMainSignal msg signalState)

                _ ->
                    model


updateDistantSignal : Msg -> { a | distantState : Msg, shortBrakePath : Bool, zs3v : Zs3.Model } -> { a | distantState : Msg, shortBrakePath : Bool, zs3v : Zs3.Model }
updateDistantSignal msg signalState =
    case msg of
        ToggleShortBrakePath ->
            { signalState | shortBrakePath = not signalState.shortBrakePath }

        ToggleHasRa12 ->
            signalState

        ToggleHasZs1 ->
            signalState

        SetZs3Absent ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3Dynamic ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3Fixed ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        SetZs3SpeedLimit _ ->
            { signalState | zs3v = Zs3.update msg signalState.zs3v }

        ToggleHasZs7 ->
            signalState

        _ ->
            { signalState | distantState = msg }


updateMainSignal : Msg -> { a | mainState : Msg, hasRa12 : Bool, hasZs1 : Bool, hasZs7 : Bool, zs3 : Zs3.Model } -> { a | mainState : Msg, hasRa12 : Bool, hasZs1 : Bool, hasZs7 : Bool, zs3 : Zs3.Model }
updateMainSignal msg signalState =
    case msg of
        ToggleShortBrakePath ->
            signalState

        ToggleHasRa12 ->
            { signalState | hasRa12 = not signalState.hasRa12 }

        ToggleHasZs1 ->
            { signalState | hasZs1 = not signalState.hasZs1 }

        SetZs3Absent ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3Dynamic ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3Fixed ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        SetZs3SpeedLimit _ ->
            { signalState | zs3 = Zs3.update msg signalState.zs3 }

        ToggleHasZs7 ->
            { signalState | hasZs7 = not signalState.hasZs7 }

        _ ->
            { signalState | mainState = msg }


ksSignal : Model -> KsSignal.Model
ksSignal model =
    { topWhiteLight = ksSignalTopWhiteLight model
    , redLight = ksSignalRedLight model
    , greenLight = ksSignalGreenLight model
    , orangeLight = ksSignalOrangeLight model
    , centerWhiteLight = ksSignalCenterWhiteLight model
    , zs7Lights = ksSignalZs7Lights model
    , bottomWhiteLight = ksSignalBottomWhiteLight model
    }


ksSignalTopWhiteLight : Model -> Lamp.State
ksSignalTopWhiteLight model =
    let
        topWhiteLight notForcedOff state =
            case state.shortBrakePath of
                False ->
                    Lamp.Absent

                True ->
                    if notForcedOff && (isExpectStop state || Zs3.hasSpeedLimit state.zs3v) then
                        Lamp.On
                    else
                        Lamp.Off
    in
        case model of
            MainSignal _ ->
                Lamp.Absent

            CombinationSignal state ->
                topWhiteLight (isProceed state) state

            DistantSignal state ->
                case state.repeater of
                    True ->
                        Lamp.Absent

                    False ->
                        topWhiteLight True state


ksSignalRedLight : Model -> Lamp.State
ksSignalRedLight model =
    case model of
        MainSignal state ->
            if isStop state then
                Lamp.On
            else
                Lamp.Off

        CombinationSignal state ->
            if isStop state then
                Lamp.On
            else
                Lamp.Off

        DistantSignal _ ->
            Lamp.Absent


ksSignalGreenLight : Model -> Lamp.State
ksSignalGreenLight model =
    let
        distant enabled state =
            if enabled && isExpectProceed state then
                if Zs3.hasSpeedLimit state.zs3v then
                    Lamp.Blinking
                else
                    Lamp.On
            else
                Lamp.Off
    in
        case model of
            MainSignal state ->
                if isProceed state then
                    Lamp.On
                else
                    Lamp.Off

            CombinationSignal state ->
                distant (isProceed state) state

            DistantSignal state ->
                distant True state


ksSignalOrangeLight : Model -> Lamp.State
ksSignalOrangeLight model =
    let
        distant enabled state =
            if enabled && isExpectStop state then
                Lamp.On
            else
                Lamp.Off
    in
        case model of
            MainSignal _ ->
                Lamp.Absent

            CombinationSignal state ->
                distant (isProceed state) state

            DistantSignal state ->
                distant True state


ksSignalCenterWhiteLight : Model -> Lamp.State
ksSignalCenterWhiteLight model =
    let
        main isMainSignal state =
            if state.hasRa12 || (isMainSignal && state.hasZs1) then
                if state.mainState == StopAndRa12 then
                    Lamp.On
                else if state.mainState == StopAndZs1 && isMainSignal then
                    Lamp.Blinking
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            MainSignal state ->
                main True state

            CombinationSignal state ->
                main False state

            DistantSignal state ->
                Lamp.Absent


ksSignalZs7Lights : Model -> Lamp.State
ksSignalZs7Lights model =
    let
        main state =
            if state.hasZs7 then
                if state.mainState == StopAndZs7 then
                    Lamp.On
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            MainSignal state ->
                main state

            CombinationSignal state ->
                main state

            DistantSignal state ->
                Lamp.Absent


ksSignalBottomWhiteLight : Model -> Lamp.State
ksSignalBottomWhiteLight model =
    let
        main isCombinationSignal state =
            if state.hasRa12 || (isCombinationSignal && state.hasZs1) then
                if state.mainState == StopAndRa12 then
                    Lamp.On
                else if state.mainState == StopAndZs1 && isCombinationSignal then
                    Lamp.Blinking
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            MainSignal state ->
                main False state

            CombinationSignal state ->
                main True state

            DistantSignal state ->
                case state.repeater of
                    False ->
                        Lamp.Absent

                    True ->
                        if isExpectStop state || Zs3.hasSpeedLimit state.zs3v then
                            Lamp.On
                        else
                            Lamp.Off


view : Model -> Svg msg
view model =
    svg [ version "1.1", viewBox "0 0 70 300", width "200" ]
        [ Svg.style []
            [ text """
                    circle { stroke-width: 0.4; stroke-opacity: 0.85; stroke: #333; }
                    .green { fill:url(#green-gradient) }
                    .green.on { filter: drop-shadow(0 0 5px #00bd4a) }
                    .red { fill:url(#red-gradient);  }
                    .red.on { filter: drop-shadow(0 0 5px #da012a) }
                    .orange { fill:url(#orange-gradient) }
                    .orange.on { filter: drop-shadow(0 0 5px #fc8e00) }
                    .yellow { fill:url(#yellow-gradient) }
                    .yellow.on { filter: drop-shadow(0 0 5px #fac412) }
                    .white { fill:url(#white-gradient) }
                    .white.on { filter: drop-shadow(0 0 5px #ebe6d8) }
                    circle.on { fill-opacity: 1; transition: fill-opacity .05s ease-in; transition-delay: .1s }
                    circle.off { fill-opacity: 0; transition: fill-opacity .2s ease-out }
                    @keyframes blinking {
                        0% { fill-opacity: 1 }
                        37.5% { fill-opacity: 1 }
                        40% { fill-opacity: 0 }
                        90% { fill-opacity: 0 }
                        100% { fill-opacity: 1 }

                    }
                    circle.blinking { animation: blinking 2s ease infinite }
                    """
            ]
        , defs []
            [ radialGradient [ id "green-gradient" ]
                [ stop [ stopColor "#33ff6d", offset "0.05" ] []
                , stop [ stopColor "#00bd4a", offset "0.9" ] []
                ]
            , radialGradient [ id "red-gradient" ]
                [ stop [ stopColor "#ff3763", offset "0.05" ] []
                , stop [ stopColor "#da012a", offset "0.9" ] []
                ]
            , radialGradient [ id "orange-gradient" ]
                [ stop [ stopColor "#ffc955", offset "0.05" ] []
                , stop [ stopColor "#fc8e00", offset "0.9" ] []
                ]
            , radialGradient [ id "yellow-gradient" ]
                [ stop [ stopColor "#ffe060", offset "0.05" ] []
                , stop [ stopColor "#fac412", offset "0.9" ] []
                ]
            , radialGradient [ id "white-gradient" ]
                [ stop [ stopColor "#fffaef", offset "0.05" ] []
                , stop [ stopColor "#ebe6d8", offset "0.9" ] []
                ]
            ]
        , g []
            [ case model of
                MainSignal signal ->
                    Zs3.view signal.zs3 (isStop signal)

                CombinationSignal signal ->
                    Zs3.view signal.zs3 (isStop signal)

                _ ->
                    g [] []
            ]
        , g [ transform "translate(3, 65)" ]
            (model |> ksSignal |> KsSignal.view)
        , g [ transform "translate(0, 168)" ]
            [ case model of
                DistantSignal signal ->
                    Zs3.view signal.zs3v (isExpectStop signal)

                CombinationSignal signal ->
                    Zs3.view signal.zs3v (isExpectStop signal || isStop signal)

                _ ->
                    g [] []
            ]
        ]


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
