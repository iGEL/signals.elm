module HvLightSignal exposing (Model, hpZs1Lights, hpZs7Lights, lights, view, zs1AndZs7View)

import Lamp
import Messages exposing (..)
import SignalModel
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { hp : Maybe HP
    , vr : Maybe VR
    }


type alias HP =
    { greenLight : Lamp.State
    , redLight : Lamp.State
    , secondaryRedLight : Lamp.State
    , ra12Lights : Lamp.State
    , orangeLight : Lamp.State
    , zs1Lights : Lamp.State
    , zs7Lights : Lamp.State
    }


type alias VR =
    { topOrangeLight : Lamp.State
    , topGreenLight : Lamp.State
    , whiteLight : Lamp.State
    , bottomOrangeLight : Lamp.State
    , bottomGreenLight : Lamp.State
    }


lights : SignalModel.Model -> Model
lights model =
    let
        speedLimitUpTo60 state =
            case state.speedLimit of
                Nothing ->
                    False

                Just speed ->
                    speed <= 6
    in
    case model of
        SignalModel.DistantSignal state ->
            { hp = Nothing
            , vr =
                Just
                    { topOrangeLight = vrTopOrangeLight state False
                    , topGreenLight = vrTopGreenLight state False
                    , whiteLight = vrWhiteLight state (state.extraLight == SignalModel.Repeater) False
                    , bottomOrangeLight = vrBottomOrangeLight state (speedLimitUpTo60 state) False
                    , bottomGreenLight = vrBottomGreenLight state (speedLimitUpTo60 state) False
                    }
            }

        SignalModel.MainSignal state ->
            { hp =
                Just
                    { greenLight = hpGreenLight state
                    , redLight = hpRedLight state
                    , secondaryRedLight = hpSecondaryRedLight state
                    , ra12Lights = hpRa12Lights state
                    , orangeLight = hpOrangeLight state (speedLimitUpTo60 state)
                    , zs1Lights = hpZs1Lights state
                    , zs7Lights = hpZs7Lights state
                    }
            , vr = Nothing
            }

        SignalModel.CombinationSignal states ->
            { hp =
                Just
                    { greenLight = hpGreenLight states.mainSignal
                    , redLight = hpRedLight states.mainSignal
                    , secondaryRedLight = hpSecondaryRedLight states.mainSignal
                    , ra12Lights = hpRa12Lights states.mainSignal
                    , orangeLight = hpOrangeLight states.mainSignal (speedLimitUpTo60 states.mainSignal)
                    , zs1Lights = hpZs1Lights states.mainSignal
                    , zs7Lights = hpZs7Lights states.mainSignal
                    }
            , vr =
                let
                    forcedOff =
                        SignalModel.isStopState states.mainSignal
                in
                Just
                    { topOrangeLight = vrTopOrangeLight states.distantSignal forcedOff
                    , topGreenLight = vrTopGreenLight states.distantSignal forcedOff
                    , whiteLight = vrWhiteLight states.distantSignal False forcedOff
                    , bottomOrangeLight = vrBottomOrangeLight states.distantSignal (speedLimitUpTo60 states.distantSignal) forcedOff
                    , bottomGreenLight = vrBottomGreenLight states.distantSignal (speedLimitUpTo60 states.distantSignal) forcedOff
                    }
            }


vrTopOrangeLight : SignalModel.StateModel -> Bool -> Lamp.State
vrTopOrangeLight state forcedOff =
    if not forcedOff && SignalModel.isStopState state then
        Lamp.On

    else
        Lamp.Off


vrTopGreenLight : SignalModel.StateModel -> Bool -> Lamp.State
vrTopGreenLight state forcedOff =
    if not forcedOff && SignalModel.isProceedState state then
        Lamp.On

    else
        Lamp.Off


vrWhiteLight : SignalModel.StateModel -> Bool -> Bool -> Lamp.State
vrWhiteLight state repeater forcedOff =
    if state.extraLight == SignalModel.ShortenedBrakePath || repeater then
        if forcedOff then
            Lamp.Off

        else
            Lamp.On

    else
        Lamp.Absent


vrBottomOrangeLight : SignalModel.StateModel -> Bool -> Bool -> Lamp.State
vrBottomOrangeLight state reducedSpeed forcedOff =
    if not forcedOff && (SignalModel.isStopState state || ((List.member 4 state.slowSpeedLights || state.hasRa12) && reducedSpeed)) then
        Lamp.On

    else
        Lamp.Off


vrBottomGreenLight : SignalModel.StateModel -> Bool -> Bool -> Lamp.State
vrBottomGreenLight state reducedSpeed forcedOff =
    if not forcedOff && SignalModel.isProceedState state && not ((List.member 4 state.slowSpeedLights || state.hasRa12) && reducedSpeed) then
        Lamp.On

    else
        Lamp.Off


hpGreenLight : SignalModel.StateModel -> Lamp.State
hpGreenLight state =
    if SignalModel.isProceedState state then
        Lamp.On

    else
        Lamp.Off


hpOrangeLight : SignalModel.StateModel -> Bool -> Lamp.State
hpOrangeLight state reducedSpeed =
    if List.member 4 state.slowSpeedLights || state.hasRa12 then
        if SignalModel.isProceedState state && reducedSpeed then
            Lamp.On

        else
            Lamp.Off

    else
        Lamp.Absent


hpRedLight : SignalModel.StateModel -> Lamp.State
hpRedLight state =
    if SignalModel.isStopState state then
        Lamp.On

    else
        Lamp.Off


hpSecondaryRedLight : SignalModel.StateModel -> Lamp.State
hpSecondaryRedLight state =
    if not state.hasRa12 then
        Lamp.Absent

    else if SignalModel.isStopState state && not (state.aspect == StopAndRa12) then
        Lamp.On

    else
        Lamp.Off


hpRa12Lights : SignalModel.StateModel -> Lamp.State
hpRa12Lights state =
    if not state.hasRa12 then
        Lamp.Absent

    else if state.aspect == StopAndRa12 then
        Lamp.On

    else
        Lamp.Off


hpZs1Lights : SignalModel.StateModel -> Lamp.State
hpZs1Lights state =
    if not state.hasZs1 then
        Lamp.Absent

    else if state.aspect == StopAndZs1 then
        Lamp.On

    else
        Lamp.Off


hpZs7Lights : SignalModel.StateModel -> Lamp.State
hpZs7Lights state =
    if not state.hasZs7 then
        Lamp.Absent

    else if state.aspect == StopAndZs7 then
        Lamp.On

    else
        Lamp.Off


view : Model -> List (Svg msg)
view model =
    let
        identity n =
            n

        mainLights =
            case model.hp of
                Just hp ->
                    let
                        hasRa12 =
                            hp.ra12Lights /= Lamp.Absent

                        yWithOffset offset =
                            (if hasRa12 then
                                offset + 134

                             else
                                offset
                                    + 145
                            )
                                |> String.fromInt
                    in
                    List.append
                        (if hasRa12 then
                            [ Just (rect [ width "58", height "134", x "22", y "0", Svg.Attributes.style "fill:black; stroke: none" ] [])
                            , Lamp.maybeBigLamp hp.greenLight "green" "38" "23"
                            , Lamp.maybeBigLamp hp.redLight "red" "38" "44"
                            , Lamp.maybeBigLamp hp.secondaryRedLight "red" "65" "44"
                            , Lamp.maybeSmallLamp hp.ra12Lights "white" "66" "68"
                            , Lamp.maybeSmallLamp hp.ra12Lights "white" "38" "92"
                            , Lamp.maybeBigLamp hp.orangeLight "orange" "38" "115"
                            ]

                         else
                            [ Just (rect [ width "65", height "145", x "18.8", y "0", Svg.Attributes.style "fill:black; stroke: none" ] [])
                            , Lamp.maybeBigLamp hp.redLight "red" "43" "126"
                            , Lamp.maybeBigLamp hp.greenLight
                                "green"
                                "60"
                                (if hp.orangeLight == Lamp.Absent then
                                    "126"

                                 else
                                    "24"
                                )
                            , Lamp.maybeBigLamp hp.orangeLight "orange" "60" "126"
                            ]
                        )
                        [ Just (g [ transform ("translate(34, " ++ yWithOffset 2 ++ ")") ] (zs1AndZs7View hp)) ]

                Nothing ->
                    []

        distantLights =
            case model.vr of
                Just vr ->
                    List.append
                        [ Just (Svg.path [ d "m 0,256.82806 0,28.15532 16.4938603,15.11883 29.3079287,0 58.748797,-60.73876 0,-28.18838 -15.93946,-14.32876 -29.307928,0 z" ] [])
                        , Lamp.maybeBigLamp vr.topOrangeLight "orange" "70" "222"
                        , Lamp.maybeBigLamp vr.topGreenLight "green" "89" "222"
                        , Lamp.maybeBigLamp vr.bottomOrangeLight "orange" "18" "275"
                        , Lamp.maybeBigLamp vr.bottomGreenLight "green" "37" "275"
                        ]
                        (if vr.whiteLight /= Lamp.Absent then
                            [ Just (Svg.path [ d "m 8,211 a 15,15 0 0 0 0,21.2132 l 10.6065997,10.6066 10.6066,-10.6066 10.6066,-10.6066 -10.6066,-10.6066 a 15,15 0 0 0 -21.2131997,0 z" ] [])
                            , Lamp.maybeSmallLamp vr.whiteLight "white" "18" "222"
                            ]

                         else
                            []
                        )

                Nothing ->
                    []
    in
    List.filterMap identity
        (List.append mainLights distantLights)


zs1AndZs7View : { a | zs1Lights : Lamp.State, zs7Lights : Lamp.State } -> List (Svg msg)
zs1AndZs7View hp =
    let
        identity =
            \n -> n
    in
    List.filterMap identity
        (if hp.zs1Lights /= Lamp.Absent || hp.zs7Lights /= Lamp.Absent then
            [ Just (rect [ width "35", height "33", x "0", y "0" ] [])
            , Lamp.maybeSmallLamp hp.zs1Lights "white" "17" "10"
            , Lamp.maybeSmallLamp hp.zs1Lights "white" "8" "24"
            , Lamp.maybeSmallLamp hp.zs1Lights "white" "26" "24"
            , Lamp.maybeSmallLamp hp.zs7Lights "orange" "8" "10"
            , Lamp.maybeSmallLamp hp.zs7Lights "orange" "26" "10"
            , Lamp.maybeSmallLamp hp.zs7Lights "orange" "17" "24"
            ]

         else
            []
        )
