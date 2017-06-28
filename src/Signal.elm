module Signal exposing (..)

import KsSignal
import HvSignal
import HlSignal
import Messages exposing (..)
import SignalModel exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Zs3


update : Messages.Target -> Model -> Model
update target model =
    case model of
        DistantSignal state ->
            case target of
                ToDistantSignal msg ->
                    DistantSignal (updateSignal msg state)

                _ ->
                    model

        CombinationSignal states ->
            case target of
                ToDistantSignal msg ->
                    CombinationSignal { states | distantSignal = updateSignal msg states.distantSignal }

                ToMainSignal msg ->
                    CombinationSignal { states | mainSignal = updateSignal msg states.mainSignal }

        MainSignal state ->
            case target of
                ToMainSignal msg ->
                    MainSignal (updateSignal msg state)

                _ ->
                    model


updateSignal : Msg -> SignalModel.StateModel -> SignalModel.StateModel
updateSignal msg state =
    case msg of
        SetAspect aspect ->
            { state | aspect = aspect }

        SetSpeedLimit maybeSpeedLimit ->
            { state | speedLimit = maybeSpeedLimit }

        SetZs3Absent ->
            { state | zs3 = Zs3.Absent }

        SetZs3Dynamic ->
            { state | zs3 = Zs3.Dynamic }

        SetZs3Fixed ->
            { state | zs3 = Zs3.Fixed }

        ToggleHasRa12 ->
            { state | hasRa12 = not state.hasRa12 }

        ToggleHasZs1 ->
            { state | hasZs1 = not state.hasZs1 }

        ToggleHasZs7 ->
            { state | hasZs7 = not state.hasZs7 }

        ToggleShortBrakePath ->
            { state
                | extraLight =
                    case state.extraLight of
                        SignalModel.ShortenedBrakePath ->
                            SignalModel.Absent

                        SignalModel.Absent ->
                            SignalModel.ShortenedBrakePath

                        SignalModel.Repeater ->
                            SignalModel.Repeater
            }

        ToggleSlowSpeedLight speed ->
            { state
                | slowSpeedLights =
                    if List.member speed state.slowSpeedLights then
                        List.filter (\s -> s /= speed) state.slowSpeedLights
                    else
                        speed :: state.slowSpeedLights
            }


view : Model -> SignalType -> Svg msg
view model signalType =
    let
        zs3 =
            case model of
                MainSignal state ->
                    Zs3.view Zs3.MainSignalLocation state.zs3 state.speedLimit (isStopState state)

                CombinationSignal states ->
                    Zs3.view Zs3.MainSignalLocation states.mainSignal.zs3 states.mainSignal.speedLimit (isStopState states.mainSignal)

                _ ->
                    g [] []

        zs3v =
            case model of
                DistantSignal state ->
                    Zs3.view Zs3.DistantSignalLocation state.zs3 state.speedLimit (isStopState state)

                CombinationSignal states ->
                    Zs3.view Zs3.DistantSignalLocation states.distantSignal.zs3 states.distantSignal.speedLimit (isStopState states.distantSignal || isStopState states.mainSignal)

                _ ->
                    g [] []
    in
        svg [ version "1.1", viewBox "0 0 110 500", width "150" ]
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
                    .hlSignal circle.blinking { animation: blinking 1s ease infinite }
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
                (case signalType of
                    Ks ->
                        [ zs3
                        , g [ transform "translate(3, 65)" ]
                            (model |> KsSignal.lights |> KsSignal.view)
                        , g [ transform "translate(0, 168)" ] [ zs3v ]
                        ]

                    HvLight ->
                        [ g [ transform "translate(19, 0)" ] [ zs3 ]
                        , g [ transform "translate(3, 65)" ]
                            (model |> HvSignal.lights |> HvSignal.view)
                        , g [ transform "translate(19, 350)" ] [ zs3v ]
                        ]

                    Hl ->
                        (model |> HlSignal.lights |> HlSignal.view)
                )
            ]
