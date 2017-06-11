module KsSignal exposing (..)

import Lamp
import Messages exposing (..)
import SignalModel
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { topWhiteLight : Lamp.State
    , redLight : Lamp.State
    , greenLight : Lamp.State
    , orangeLight : Lamp.State
    , centerWhiteLight : Lamp.State
    , zs7Lights : Lamp.State
    , bottomWhiteLight : Lamp.State
    }


lights : SignalModel.Model -> Model
lights model =
    { topWhiteLight = topWhiteLight model
    , redLight = redLight model
    , greenLight = greenLight model
    , orangeLight = orangeLight model
    , centerWhiteLight = centerWhiteLight model
    , zs7Lights = zs7Lights model
    , bottomWhiteLight = bottomWhiteLight model
    }


topWhiteLight : SignalModel.Model -> Lamp.State
topWhiteLight model =
    let
        topWhiteLight enabled state =
            case state.extraLight of
                SignalModel.ShortenedBrakePath ->
                    if enabled && (SignalModel.isStopState state || SignalModel.isSpeedLimitState state) then
                        Lamp.On
                    else
                        Lamp.Off

                _ ->
                    Lamp.Absent
    in
        case model of
            SignalModel.MainSignal _ ->
                Lamp.Absent

            SignalModel.CombinationSignal states ->
                topWhiteLight (SignalModel.isProceed model) states.distantSignal

            SignalModel.DistantSignal state ->
                topWhiteLight True state


redLight : SignalModel.Model -> Lamp.State
redLight model =
    case model of
        SignalModel.MainSignal state ->
            if SignalModel.isStopState state then
                Lamp.On
            else
                Lamp.Off

        SignalModel.CombinationSignal states ->
            if SignalModel.isStopState states.mainSignal then
                Lamp.On
            else
                Lamp.Off

        SignalModel.DistantSignal _ ->
            Lamp.Absent


greenLight : SignalModel.Model -> Lamp.State
greenLight model =
    let
        distant enabled state =
            if enabled && SignalModel.isProceedState state then
                if SignalModel.isSpeedLimitState state then
                    Lamp.Blinking
                else
                    Lamp.On
            else
                Lamp.Off
    in
        case model of
            SignalModel.MainSignal state ->
                if SignalModel.isProceedState state then
                    Lamp.On
                else
                    Lamp.Off

            SignalModel.CombinationSignal states ->
                distant (SignalModel.isProceedState states.mainSignal) states.distantSignal

            SignalModel.DistantSignal state ->
                distant True state


orangeLight : SignalModel.Model -> Lamp.State
orangeLight model =
    let
        distant enabled state =
            if enabled && SignalModel.isStopState state then
                Lamp.On
            else
                Lamp.Off
    in
        case model of
            SignalModel.MainSignal _ ->
                Lamp.Absent

            SignalModel.CombinationSignal states ->
                distant (SignalModel.isProceedState states.mainSignal) states.distantSignal

            SignalModel.DistantSignal state ->
                distant True state


centerWhiteLight : SignalModel.Model -> Lamp.State
centerWhiteLight model =
    let
        main isMainSignal state =
            if state.hasRa12 || (isMainSignal && state.hasZs1) then
                if state.aspect == StopAndRa12 then
                    Lamp.On
                else if state.aspect == StopAndZs1 && isMainSignal then
                    Lamp.Blinking
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            SignalModel.MainSignal state ->
                main True state

            SignalModel.CombinationSignal states ->
                main False states.mainSignal

            SignalModel.DistantSignal state ->
                Lamp.Absent


zs7Lights : SignalModel.Model -> Lamp.State
zs7Lights model =
    let
        main state =
            if state.hasZs7 then
                if state.aspect == StopAndZs7 then
                    Lamp.On
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            SignalModel.MainSignal state ->
                main state

            SignalModel.CombinationSignal states ->
                main states.mainSignal

            SignalModel.DistantSignal state ->
                Lamp.Absent


bottomWhiteLight : SignalModel.Model -> Lamp.State
bottomWhiteLight model =
    let
        main isCombinationSignal state =
            if state.hasRa12 || (isCombinationSignal && state.hasZs1) then
                if state.aspect == StopAndRa12 then
                    Lamp.On
                else if state.aspect == StopAndZs1 && isCombinationSignal then
                    Lamp.Blinking
                else
                    Lamp.Off
            else
                Lamp.Absent
    in
        case model of
            SignalModel.MainSignal state ->
                main False state

            SignalModel.CombinationSignal states ->
                main True states.mainSignal

            SignalModel.DistantSignal state ->
                case state.extraLight of
                    SignalModel.Repeater ->
                        if SignalModel.isStopState state || SignalModel.isSpeedLimitState state then
                            Lamp.On
                        else
                            Lamp.Off

                    _ ->
                        Lamp.Absent


view : Model -> List (Svg msg)
view model =
    let
        identity n =
            n
    in
        (List.filterMap
            identity
            [ Just (rect [ width "64", height "120", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] [])
            , Lamp.maybeSmallLamp model.topWhiteLight "white" "16.5" "14.5"
            , Lamp.maybeBigLamp model.redLight "red" "32" "32"
            , Lamp.maybeBigLamp model.greenLight
                "green"
                (if model.orangeLight == Lamp.Absent then
                    "32"
                 else
                    "16.5"
                )
                "57.3"
            , Lamp.maybeBigLamp model.orangeLight "orange" "47.5" "57.3"
            , Lamp.maybeSmallLamp model.centerWhiteLight "white" "32" "81"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "21.5" "81"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "32" "98.7"
            , Lamp.maybeSmallLamp model.zs7Lights "yellow" "42.5" "81"
            , Lamp.maybeSmallLamp model.bottomWhiteLight "white" "11.5" "98.7"
            ]
        )
