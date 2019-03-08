module HvSemaphore exposing (Model, arms, view)

import Arm
import HvLightSignal exposing (hpZs1Lights, hpZs7Lights)
import Lamp
import Messages exposing (..)
import Ne2
import SignalModel
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { hp : Maybe HP
    , sh : Arm.State
    , vr : Maybe VR
    }


type alias HP =
    { topArm : Arm.State
    , lowerArm : Arm.State
    , zs1Lights : Lamp.State
    , zs7Lights : Lamp.State
    }


type alias VR =
    { disk : Arm.State
    , arm : Arm.State
    , rightLights : Arm.State
    , shortBreakPathSign : Bool
    }


arms : SignalModel.Model -> Model
arms model =
    let
        speedLimitUpTo60 state =
            case state.speedLimit of
                Nothing ->
                    False

                Just speed ->
                    speed <= 6

        mainTopArm state =
            if SignalModel.isStopState state then
                Arm.Horizontal

            else
                Arm.Inclined

        slowSpeedArm state forcedStop =
            if List.member 4 state.slowSpeedLights then
                if speedLimitUpTo60 state && not forcedStop && SignalModel.isProceedState state then
                    Arm.Inclined

                else
                    Arm.Vertical

            else
                Arm.Absent

        distantDisk state forcedStop =
            if SignalModel.isStopState state || forcedStop || (speedLimitUpTo60 state && List.member 4 state.slowSpeedLights) then
                Arm.Vertical

            else
                Arm.Horizontal

        rightLights state forcedStop =
            if SignalModel.isStopState state || forcedStop then
                Arm.Vertical

            else
                Arm.Inclined

        sh state =
            if state.hasRa12 then
                if SignalModel.isStopState state && not (state.aspect == StopAndRa12) then
                    Arm.Horizontal

                else
                    Arm.Inclined

            else
                Arm.Absent
    in
    case model of
        SignalModel.DistantSignal state ->
            if state.extraLight == SignalModel.Repeater then
                { hp = Nothing, sh = Arm.Absent, vr = Nothing }

            else
                { hp = Nothing
                , sh = Arm.Absent
                , vr =
                    Just
                        { disk = distantDisk state False
                        , arm = slowSpeedArm state False
                        , rightLights = rightLights state False
                        , shortBreakPathSign = state.extraLight == SignalModel.ShortenedBrakePath
                        }
                }

        SignalModel.CombinationSignal states ->
            { hp =
                Just
                    { topArm = mainTopArm states.mainSignal
                    , lowerArm = slowSpeedArm states.mainSignal False
                    , zs1Lights = hpZs1Lights states.mainSignal
                    , zs7Lights = hpZs7Lights states.mainSignal
                    }
            , sh = sh states.mainSignal
            , vr =
                Just
                    { disk = distantDisk states.distantSignal (SignalModel.isStopState states.mainSignal)
                    , arm = slowSpeedArm states.distantSignal (SignalModel.isStopState states.mainSignal)
                    , rightLights = rightLights states.distantSignal (SignalModel.isStopState states.mainSignal)
                    , shortBreakPathSign = states.distantSignal.extraLight == SignalModel.ShortenedBrakePath
                    }
            }

        SignalModel.MainSignal state ->
            { hp =
                Just
                    { topArm = mainTopArm state
                    , lowerArm = slowSpeedArm state False
                    , zs1Lights = hpZs1Lights state
                    , zs7Lights = hpZs7Lights state
                    }
            , sh = sh state
            , vr = Nothing
            }


view : Model -> List (Svg msg)
view model =
    [ g [ transform "translate(0 20)" ]
        (List.concat
            [ case model.hp of
                Nothing ->
                    []

                Just state ->
                    let
                        gridCross =
                            [ Svg.path [ transform "rotate(45)", d "M295.29 299.044h17.644v2.474H295.29z" ] []
                            , Svg.path [ transform "rotate(-45)", d "M-309.103 302.875h17.644v2.474h-17.644z" ] []
                            ]
                    in
                    [ Svg.path [ d "M3.597 12.045h14.347v19.176H3.597z", class "ral6011" ] []
                    , Svg.path [ d "M3.597-11.218h2.474v28.995H3.597z", class "ral6011" ] []
                    , Svg.path [ d "M3.597-13.345h14.347v2.474H3.597z", class "ral6011" ] []
                    , Svg.path [ d "M15.506-12.182h2.474v29.226h-2.474z", class "ral6011" ] []
                    , Svg.path [ d "M3.597 392h2.474v108H3.597z", class "ral6011" ] []
                    , Svg.path [ d "M15.453 392h2.474v108h-2.474z", class "ral6011" ] []
                    , g [ transform "translate(8.1)", class "ral6011" ] gridCross
                    , g [ transform "translate(8.1 16)", class "ral6011" ] gridCross
                    , g [ transform "translate(8.51 32)", class "ral6011" ] gridCross
                    , g [ transform "translate(8.184 48)", class "ral6011" ] gridCross
                    , g [ transform "translate(8.184 64)", class "ral6011" ] gridCross
                    , g
                        [ if state.topArm == Arm.Inclined then
                            Svg.Attributes.style "transform: rotate(-45deg)"

                          else
                            Svg.Attributes.style "transform: translate(36px, 0px)"
                        , class "semaphoreAnimate"
                        ]
                        [ g [ transform "rotate(33.832)" ]
                            -- topLights
                            [ circle [ cx "25.785", cy "28.549", r "9.874", class "ral9007" ] []
                            , circle [ cx "25.785", cy "49.448", r "9.874", class "ral9007" ] []
                            , Svg.path [ d "M15.912 28.549h19.747v20.9H15.912z", class "ral9007" ] []
                            , Svg.path [ transform "rotate(-33.832)", d "M-21.741 41.036H-5.32v4.913h-16.421z", class "ral9007" ] []
                            , circle [ cx "25.785", cy "28.549", r "7.152", fill "#e64e54" ] []
                            , circle [ cx "25.785", cy "49.446", r "7.152", fill "#2cecf5" ] []
                            ]
                        ]
                    , g
                        [ if state.lowerArm == Arm.Inclined then
                            Svg.Attributes.style "transform: translate(0, 155px) rotate(-45deg)"

                          else
                            Svg.Attributes.style "transform: translate(36px, 155px)"
                        , class "semaphoreAnimate"
                        ]
                        (if state.lowerArm == Arm.Absent then
                            []

                         else
                            [ g [ transform "rotate(33.832)" ]
                                [ Svg.path [ d "M15.912 28.549h19.747v20.9H15.912z", class "ral9007" ] []
                                , Svg.path [ transform "rotate(-33.832)", d "M-21.546 36.709h16.697v5.459h-16.697z", class "ral9007" ] []
                                , circle [ cx "25.785", cy "28.549", r "9.874", class "ral9007" ] []
                                , circle [ cx "25.785", cy "49.448", r "9.874", class "ral9007" ] []
                                , circle [ cx "25.785", cy "49.446", r "7.152", fill "#db9b6e" ] []
                                ]
                            ]
                        )
                    , g [ transform "translate(8.1)" ]
                        [ Svg.path [ d "M-4.503 31.078H9.844v361.835H-4.503z", class "ral3002" ] []
                        , Svg.path [ d "M-4.503 247.544H9.844v72.841H-4.503z", class "ral9002" ] []
                        , Svg.path [ d "M-4.503 103.409H9.844v71.736H-4.503z", class "ral9002" ] []
                        ]
                    , Svg.path [ d "M3.597 398.5h14.347v21.981H3.597z", class "ral9002" ] []
                    , g
                        -- topArm
                        [ if state.topArm == Arm.Inclined then
                            Svg.Attributes.style "transform: translate(10.98px, 11.225px) rotate(-45deg)"

                          else
                            Svg.Attributes.style "transform: translate(10.98px, 11.225px)"
                        , class "semaphoreAnimate"
                        ]
                        [ g
                            [ transform "translate(-10.98 -11.225)" ]
                            [ rect [ width "133.13008", height "16", x "-19.884901", y "3.20714", class "ral3002" ] []
                            , rect [ width "125.01228", height "6.8530164", x "-15.215975", y "7.7495484", class "ral9002" ] []
                            , circle [ cx "124.86", cy "11.034", r "15.97", class "ral3002" ] []
                            , circle [ cx "124.86", cy "11.034", r "9.805", class "ral9002" ] []
                            , circle [ cx "10.98", cy "11.225", r "1.8", class "screw" ] []
                            ]
                        ]
                    , g
                        -- lowerArm
                        [ if state.lowerArm == Arm.Inclined then
                            Svg.Attributes.style "transform: translate(11.225px, 164px) rotate(-45deg)"

                          else
                            Svg.Attributes.style "transform: translate(11.225px, 164px) rotate(-90deg)"
                        , class "semaphoreAnimate"
                        ]
                        (if state.lowerArm == Arm.Absent then
                            []

                         else
                            [ g [ transform "translate(-23.039518 -11.225)" ]
                                [ rect [ width "110.1709", height "16", x "-5.0257163", y "3.20714", class "ral3002" ] []
                                , rect [ width "102.79974", height "6.8530164", x "-1.1034304", y "7.7495484", class "ral9002" ] []
                                , circle [ cx "116.75959", cy "11.034", r "15.97", class "ral3002" ] []
                                , circle [ cx "116.75959", cy "11.034", r "9.805", class "ral9002" ] []
                                , circle [ cx "23.039518", cy "11.225", r "1.8", class "screw" ] []
                                ]
                            ]
                        )
                    , g
                        [ if state.lowerArm == Arm.Absent then
                            transform "translate(-7 100)"

                          else
                            transform "translate(-7 225)"
                        ]
                        (HvLightSignal.zs1AndZs7View state)
                    ]
            , case model.vr of
                Nothing ->
                    []

                Just state ->
                    [ rect [ width "7.4288821", height "185.0302", x "61.986767", y "258.98486", class "ral6011" ] []
                    , g
                        [ transform "translate(60 278)"
                        ]
                        [ g
                            [ if state.rightLights == Arm.Inclined then
                                Svg.Attributes.style "transform: rotate(-45deg) translate(-35px, -8px)"

                              else
                                Svg.Attributes.style ""
                            , class "semaphoreAnimate"
                            ]
                            -- right vr lights
                            [ circle [ cx "25.785", cy "28.549", r "9.874", class "ral9007" ] []
                            , circle [ cx "25.785", cy "49.448", r "9.874", class "ral9007" ] []
                            , Svg.path [ d "M15.912 28.549h19.747v20.9H15.912z", class "ral9007" ] []
                            , Svg.path [ transform "rotate(-33.832)", d "M-21.741 41.036H-5.32v4.913h-16.421z", class "ral9007" ] []
                            , circle [ cx "25.785", cy "28.549", r "7.152", fill "#db9b6e" ] []
                            , circle [ cx "25.785", cy "49.446", r "7.152", fill "#2cecf5" ] []
                            ]
                        ]
                    , g
                        [ transform "translate(71 400)" ]
                        [ g
                            [ if state.disk == Arm.Horizontal then
                                Svg.Attributes.style "transform: rotate(135deg) translate(-35px, -8px)"

                              else
                                Svg.Attributes.style "transform: rotate(180deg) translate(0px, 0px)"
                            , class "semaphoreAnimate"
                            ]
                            -- left vr lights
                            [ circle [ cx "25.785", cy "28.549", r "9.874", class "ral9007" ] []
                            , circle [ cx "25.785", cy "49.448", r "9.874", class "ral9007" ] []
                            , Svg.path [ d "M15.912 28.549h19.747v20.9H15.912z", class "ral9007" ] []
                            , Svg.path [ transform "rotate(-33.832)", d "M-21.741 41.036H-5.32v4.913h-16.421z", class "ral9007" ] []
                            , circle [ cx "25.785", cy "28.549", r "7.152", fill "#db9b6e" ] []
                            , circle [ cx "25.785", cy "49.446", r "7.152", fill "#2cecf5" ] []
                            ]
                        ]
                    , g [ transform "translate(66,259)" ]
                        [ g
                            [ if state.disk == Arm.Horizontal then
                                Svg.Attributes.style "transform: rotateX(80deg)"

                              else
                                Svg.Attributes.style ""
                            , class "semaphoreAnimate"
                            ]
                            [ circle [ cx "0", cy "0", r "37.588001", class "ral9002" ] []
                            , circle [ cx "0", cy "0", r "32.56472", class "ral9005" ] []
                            , circle [ cx "0", cy "0", r "28.061089", class "ral2000" ] []
                            ]
                        ]
                    , g [ transform "translate(66,358)" ]
                        (if state.arm == Arm.Absent then
                            []

                         else
                            [ g
                                [ if state.arm == Arm.Inclined then
                                    Svg.Attributes.style "transform: rotate(-45deg)"

                                  else
                                    Svg.Attributes.style ""
                                , class "semaphoreAnimate"
                                ]
                                [ Svg.path [ class "ral9002", d "m -9,-55 18,0 0,100 -9,9 -9,-9 z" ] []
                                , Svg.path
                                    [ class "ral2000"
                                    , Svg.Attributes.style "stroke:#0e0e10;stroke-width:2;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4"
                                    , d "m -5,-51 10,0 0,94 -5,5 -5,-5 z"
                                    ]
                                    []
                                , circle [ cx "0", cy "0", r "1.7685205", class "screw" ] []
                                ]
                            ]
                        )
                    , g [ transform "translate(48 445)" ] [ Ne2.ne2 state.shortBreakPathSign ]
                    ]
            , if model.sh == Arm.Absent then
                []

              else
                [ g []
                    [ rect [ x "-17", y "443", width "56", height "56", class "ral9005" ] []
                    , circle [ cx "10", cy "470.5", r "20.5", class "ral9002" ] []
                    , g [ transform "translate(11 471)" ]
                        [ rect
                            [ x "-23"
                            , y "-7.5"
                            , width "46"
                            , height "15"
                            , class "ral9005 semaphoreAnimate"
                            , if model.sh == Arm.Inclined then
                                Svg.Attributes.style "transform: rotate(-45deg)"

                              else
                                Svg.Attributes.style ""
                            ]
                            []
                        ]
                    ]
                ]
            ]
        )
    ]
