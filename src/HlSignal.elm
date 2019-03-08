module HlSignal exposing (lights, view)

import Lamp
import Messages
import SignalModel
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { topOrangeLight : Lamp.State
    , greenLight : Lamp.State
    , topWhiteLight : Lamp.State
    , redLight : Lamp.State
    , bottomWhiteLight : Lamp.State
    , bottomOrangeLight : Lamp.State
    , secondaryRedLight : Lamp.State
    , greenStripe : Lamp.State
    , orangeStripe : Lamp.State
    }


lights : SignalModel.Model -> Model
lights model =
    { topOrangeLight = topOrangeLight model
    , greenLight = greenLight model
    , topWhiteLight = topWhiteLight model
    , redLight = redLight model
    , bottomWhiteLight = bottomWhiteLight model
    , bottomOrangeLight = bottomOrangeLight model
    , secondaryRedLight = secondaryRedLight model
    , greenStripe = stripe model 10
    , orangeStripe = stripe model 6
    }



-- A main signal behaves the same like a combination signal without the top orange light where the next main signal is always Proceed


combinationSignalFromMain : SignalModel.StateModel -> { mainSignal : SignalModel.StateModel, distantSignal : SignalModel.StateModel }
combinationSignalFromMain mainSignalState =
    let
        setToProceed state =
            { state | aspect = Messages.Proceed }
    in
    { mainSignal = mainSignalState
    , distantSignal = setToProceed SignalModel.defaultStateModel
    }


topOrangeLight : SignalModel.Model -> Lamp.State
topOrangeLight model =
    let
        isReducedSpeedState state =
            state.speedLimit
                == Just 4
                || (state.speedLimit == Just 6 && List.member 6 state.slowSpeedLights)

        fromState state =
            if SignalModel.isStopState state then
                Lamp.On

            else if isReducedSpeedState state then
                Lamp.Blinking

            else
                Lamp.Off
    in
    case model of
        SignalModel.DistantSignal state ->
            fromState state

        SignalModel.MainSignal _ ->
            Lamp.Absent

        SignalModel.CombinationSignal states ->
            if SignalModel.isStopState states.mainSignal then
                Lamp.Off

            else
                fromState states.distantSignal


greenLight : SignalModel.Model -> Lamp.State
greenLight model =
    let
        speedIsReducedToLessThanHunderedState state =
            state.speedLimit == Just 4 || (List.member 6 state.slowSpeedLights && state.speedLimit == Just 6)

        distantFromState state =
            if SignalModel.isStopState state || speedIsReducedToLessThanHunderedState state then
                Lamp.Off

            else if state.speedLimit == Just 10 && List.member 10 state.slowSpeedLights then
                Lamp.Blinking

            else
                Lamp.On

        combinedFromState states =
            if SignalModel.isStopState states.mainSignal then
                Lamp.Off

            else
                distantFromState states.distantSignal
    in
    case model of
        SignalModel.DistantSignal state ->
            distantFromState state

        SignalModel.MainSignal state ->
            combinationSignalFromMain state
                |> combinedFromState

        SignalModel.CombinationSignal states ->
            combinedFromState states


topWhiteLight : SignalModel.Model -> Lamp.State
topWhiteLight model =
    let
        fromState state =
            if not state.hasRa12 then
                Lamp.Absent

            else if state.aspect == Messages.StopAndRa12 then
                Lamp.On

            else
                Lamp.Off
    in
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal state ->
            fromState state

        SignalModel.CombinationSignal states ->
            fromState states.mainSignal


redLight : SignalModel.Model -> Lamp.State
redLight model =
    let
        fromState state =
            if SignalModel.isStopState state then
                Lamp.On

            else
                Lamp.Off
    in
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal state ->
            fromState state

        SignalModel.CombinationSignal states ->
            fromState states.mainSignal


bottomWhiteLight : SignalModel.Model -> Lamp.State
bottomWhiteLight model =
    let
        fromState state =
            if not state.hasZs1 && not state.hasRa12 then
                Lamp.Absent

            else if state.aspect == Messages.StopAndZs1 then
                Lamp.Blinking

            else if state.aspect == Messages.StopAndRa12 then
                Lamp.On

            else
                Lamp.Off
    in
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal state ->
            fromState state

        SignalModel.CombinationSignal states ->
            fromState states.mainSignal


bottomOrangeLight : SignalModel.Model -> Lamp.State
bottomOrangeLight model =
    let
        isReducedSpeedState state =
            state.speedLimit
                == Just 4
                || (state.speedLimit == Just 6 && List.member 6 state.slowSpeedLights)
                || (state.speedLimit == Just 10 && List.member 10 state.slowSpeedLights)

        fromState state =
            if SignalModel.isProceedState state && isReducedSpeedState state then
                Lamp.On

            else
                Lamp.Off
    in
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal state ->
            fromState state

        SignalModel.CombinationSignal states ->
            fromState states.mainSignal


secondaryRedLight : SignalModel.Model -> Lamp.State
secondaryRedLight model =
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal _ ->
            Lamp.Off

        SignalModel.CombinationSignal _ ->
            Lamp.Off


stripe : SignalModel.Model -> Int -> Lamp.State
stripe model speed =
    let
        fromState state =
            if List.member speed state.slowSpeedLights then
                if SignalModel.isProceedState state && state.speedLimit == Just speed then
                    Lamp.On

                else
                    Lamp.Off

            else
                Lamp.Absent
    in
    case model of
        SignalModel.DistantSignal _ ->
            Lamp.Absent

        SignalModel.MainSignal state ->
            fromState state

        SignalModel.CombinationSignal states ->
            fromState states.mainSignal


view : Model -> List (Svg msg)
view model =
    let
        identity n =
            n

        greenStripeYPos =
            if model.orangeStripe == Lamp.Absent then
                "227"

            else
                "200"
    in
    [ g [ class "hlSignal" ]
        (List.filterMap
            identity
            [ Just
                (Svg.path
                    [ d
                        (if model.redLight == Lamp.Absent then
                            "M -0,9 8,0 62,0 l 9,9 0,122 -72,0 z"

                         else
                            "M 0,9 8,0 62,0 l 9,9 0,175 -72,0 z"
                        )
                    ]
                    []
                )
            , Lamp.maybeBigLamp model.topOrangeLight "orange" "20" "30"
            , Lamp.maybeBigLamp model.greenLight "green" "50" "30"
            , Lamp.maybeSmallLamp model.topWhiteLight "white" "53" "63"
            , Lamp.maybeBigLamp model.redLight "red" "35" "82"
            , Lamp.maybeSmallLamp model.bottomWhiteLight "white" "14" "99"
            , Lamp.maybeBigLamp model.bottomOrangeLight "orange" "20" "136"
            , Lamp.maybeBigLamp model.secondaryRedLight "red" "50" "136"
            , if model.greenStripe == Lamp.Absent && model.orangeStripe == Lamp.Absent then
                Nothing

              else
                Just (rect [ height "62", width "71", x "0", y "185" ] [])
            , Lamp.maybeSmallLamp model.greenStripe "green" "8" greenStripeYPos
            , Lamp.maybeSmallLamp model.greenStripe "green" "27" greenStripeYPos
            , Lamp.maybeSmallLamp model.greenStripe "green" "45" greenStripeYPos
            , Lamp.maybeSmallLamp model.greenStripe "green" "63" greenStripeYPos
            , Lamp.maybeSmallLamp model.orangeStripe "orange" "8" "227"
            , Lamp.maybeSmallLamp model.orangeStripe "orange" "27" "227"
            , Lamp.maybeSmallLamp model.orangeStripe "orange" "45" "227"
            , Lamp.maybeSmallLamp model.orangeStripe "orange" "63" "227"
            ]
        )
    ]
