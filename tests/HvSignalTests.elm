module HvSignalTests exposing (all)

import Expect
import HvLightSignal
import HvSemaphore
import Lamp
import Arm
import Messages exposing (..)
import Signal
import SignalModel
import Test exposing (..)


all : Test
all =
    describe "HV Signal"
        [ test "no semaphore repeater" <|
            expectedSemaphoreAfterAppliedTo SignalModel.signalRepeater
                []
                { hp = Nothing, sh = Arm.Absent, vr = Nothing }
        , describe "Stop"
            (let
                messages =
                    [ SetAspect Stop ]
             in
                [ describe "distant signal"
                    [ test "shows two orange lights for light signal - Vr0" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr0Light }
                    , test "shows orange disk for semaphore - Vr0" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                    ]
                , describe "repeater"
                    [ test "shows two orange lights for a repeater - Vr0" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr0LightKennlicht }
                    ]
                , describe "combination signal"
                    [ describe "distant stop"
                        [ test "shows main red light and turned off distant - Hp 0" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Stop ]
                                messages
                                { hp = hp0LightNoOrangeLight, vr = vrLightAllOff }
                        , test "shows orange disk and horizontal arm for semaphore - Hp0/Vr0" <|
                            expectedSemaphoreAfterAppliedToCombination messages
                                messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        ]
                    , describe "distant proceed"
                        [ test "shows orange disk and horizontal arm for semaphore - Hp0/Vr0" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        ]
                    , describe "distant proceed with speed limit"
                        [ test "shows orange disk and horizontal arm for semaphore - Hp0/Vr0" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
                                messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr0SemaphoreWithArm }
                        ]
                    ]
                , describe "main signal"
                    [ test "shows red light for light signal - Hp0" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp0LightNoOrangeLight, vr = Nothing }
                    , test "shows horizontal arm when applied to semaphore - Hp0 " <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp0SemaphoreNoLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Stop (with meaningless speed limit)"
            (let
                messages =
                    [ SetAspect Stop, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
             in
                [ describe "light signal"
                    [ test "shows two orange lights for distant signal - Vr0" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr0Light }
                    , test "shows two orange lights for a repeater - Vr0" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr0LightKennlicht }
                    , test "shows main red light and turned off distant - Hp 0" <|
                        expectedLightsAfterAppliedToCombination [ SetAspect Stop ]
                            messages
                            { hp = hp0LightWithOrangeLight, vr = vrLightAllOff }
                    , test "shows red light for main signal - Hp0" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp0LightWithOrangeLight, vr = Nothing }
                    ]
                , describe "semaphore"
                    [ test "shows orange disk for distant signal - Vr0" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreWithArm }
                    , describe "distant stop"
                        [ test "shows orange disk and horizontal arm for combination = Hp0/Vr0" <|
                            expectedSemaphoreAfterAppliedToCombination messages
                                messages
                                { hp = hp0SemaphoreWithLowerArm, sh = Arm.Absent, vr = vr0SemaphoreWithArm }
                        ]
                    , test "shows horizontal arm when applied to main signal - Hp0 " <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp0SemaphoreWithLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Proceed"
            (let
                messages =
                    [ SetAspect Proceed ]
             in
                [ describe "distant signal"
                    [ test "shows two green lights for light signal - Vr1" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr1Light }
                    , test "shows flipped away orange disk for semaphore - Vr1" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                    ]
                , describe "repeater"
                    [ test "shows two green lights for repeater - Vr1" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr1LightKennlicht }
                    ]
                , describe "combination signal"
                    [ describe "distant stop"
                        (let
                            distantMessages =
                                [ SetAspect Stop ]
                         in
                            [ test "shows green main light and two orange distant lights for light signal - Hp1/Vr0" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr0Light }
                            , test "shows inclined top arm and vertical orange disk for semaphore - Hp1/Vr0" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                            ]
                        )
                    , describe "distant proceed"
                        (let
                            distantMessages =
                                [ SetAspect Proceed ]
                         in
                            [ test "shows green main light and two green distant lights for light signal - Hp1/Vr1" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr1Light }
                            , test "shows inclined top arm and flipped away orange disk for semaphore - Hp1/Vr1" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                            ]
                        )
                    , describe "distant proceed without slow speed capability but limit below 70 km/h"
                        (let
                            distantMessages =
                                [ SetAspect Proceed, SetSpeedLimit (Just 6) ]
                         in
                            [ test "shows green main light and two green distant lights for light signal - Hp1/Vr1" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr1Light }
                            , test "shows inclined top arm and flipped away orange disk for semaphore - Hp1/Vr1" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                            ]
                        )
                    , describe "distant proceed with slow speed capability and no speed limit"
                        (let
                            distantMessages =
                                [ SetAspect Proceed, ToggleSlowSpeedLight 4 ]
                         in
                            [ test "shows green main light and two green distant lights for light signal - Hp1/Vr1" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr1Light }
                            , test "shows inclined top arm and flipped away orange disk and vertical arm for semaphore - Hp1/Vr1" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                            ]
                        )
                    , describe "distant proceed with slow speed capability and limit above 60 km/h"
                        (let
                            distantMessages =
                                [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 7) ]
                         in
                            [ test "shows green main light and two green distant lights for light signal - Hp1/Vr1" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr1Light }
                            , test "shows inclined top arm and flipped away orange disk and vertical arm for semaphore - Hp1/Vr1" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                            ]
                        )
                    , describe "distant proceed with slow speed capability and limit below 70 km/h"
                        (let
                            distantMessages =
                                [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 6) ]
                         in
                            [ test "shows green main light and green/orange distant lights for light signal - Hp1/Vr2" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr2Light }
                            , test "shows inclined top arm and flipped away orange disk and inclined arm for semaphore - Hp1/Vr2" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr2Semaphore }
                            ]
                        )
                    , describe "distant proceed without slow speed capability but for main signal with Ra12 and limit below 70 km/h"
                        (let
                            distantMessages =
                                [ SetAspect Proceed, ToggleHasRa12, SetSpeedLimit (Just 6) ]
                         in
                            [ test "shows green main light and green/orange distant lights for light signal - Hp1/Vr2" <|
                                expectedLightsAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1LightNoOrangeLight, vr = vr2Light }
                            , test "shows inclined top arm and flipped away orange disk and NO arm for semaphore - Hp1/Vr1" <|
                                expectedSemaphoreAfterAppliedToCombination distantMessages
                                    messages
                                    { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                            ]
                        )
                    ]
                , describe "main signal"
                    [ test "shows green light for light signal - Hp1" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp1LightNoOrangeLight, vr = Nothing }
                    , test "shows inclined top arm for semaphore - Hp1" <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Proceed with slow speed capability, but no limit set"
            (let
                messages =
                    [ SetAspect Proceed, ToggleSlowSpeedLight 4 ]
             in
                [ describe "light signal"
                    [ test "shows two green lights for distant signal - Vr1" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr1Light }
                    , test "shows two green lights & white light - Vr1" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr1LightKennlicht }
                    , describe "distant proceed with slow speed capability, but no limit set"
                        [ test "shows green main light & two green distant lights for combination - Hp1/Vr1" <|
                            expectedLightsAfterAppliedToCombination messages
                                messages
                                { hp = hp1LightWithOrangeLight, vr = vr1Light }
                        ]
                    , test "shows green light for main signal - Hp1" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp1LightWithOrangeLight, vr = Nothing }
                    ]
                , describe "semaphore"
                    [ test "shows flipped away orange disk for distant signal - Vr1" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                    , describe "distant proceed"
                        [ test "shows inclined top arm and flipped away orange disk for combination - Hp1/Vr1" <|
                            expectedSemaphoreAfterAppliedToCombination messages
                                messages
                                { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                        ]
                    , describe "distant stop"
                        [ test "shows inclined top arm and vertical orange disk for combination - Hp1/Vr0" <|
                            expectedSemaphoreAfterAppliedToCombination [ ToggleSlowSpeedLight 4, SetAspect Stop ]
                                messages
                                { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = vr0SemaphoreWithArm }
                        ]
                    , test "shows inclined top arm for main signal - Hp1" <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Proceed with slow speed capability and speed above 60 km/h"
            (let
                messages =
                    [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 7) ]
             in
                [ describe "light signal"
                    [ test "shows two green lights for distant - Vr1" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr1Light }
                    , test "shows two green lights for repeater - Vr1" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr1LightKennlicht }
                    , describe "distant proceed"
                        [ test "shows green main light and two green distant lights - Hp1/Vr1" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp1LightWithOrangeLight, vr = vr1Light }
                        ]
                    , describe "distant proceed with speed limit"
                        [ test "shows green main light and green/orange distant lights - Hp1/Vr2" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
                                messages
                                { hp = hp1LightWithOrangeLight, vr = vr2Light }
                        ]
                    , test "shows green light for main signal - Hp1" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp1LightWithOrangeLight, vr = Nothing }
                    ]
                , describe "semaphore"
                    [ test "shows flipped away disk for distant - Vr1" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                    , describe "distant proceed"
                        [ test "shows main arm inclined, lower arm vertical and orange disk flipped away - Hp1/Vr1" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4 ]
                                messages
                                { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = vr1SemaphoreWithArm }
                        ]
                    , describe "distant proceed with speed limit"
                        [ test "shows inclined main arm, lower arm vertical, disk horizontal and distant arm inclined - Hp1/Vr2" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
                                messages
                                { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = vr2Semaphore }
                        ]
                    , test "shows inclined main arm and lower arm vertical - Hp1" <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp1SemaphoreWithLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Proceed with slow speed capability and a limit below 70 km/h"
            (let
                messages =
                    [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 6) ]
             in
                [ describe "light signal"
                    [ test "shows a green and orange light for distant signal - Vr2" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr2Light }
                    , test "shows a green and orange light plus Kennlicht for repeater - Vr2" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr2LightKennlicht }
                    , describe "distant proceed"
                        [ test "shows green and orange main lights and both green distant lights - Hp2/Vr1" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp2Light, vr = vr1Light }
                        ]
                    , test "shows green and orange light - Hp2" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp2Light, vr = Nothing }
                    ]
                , describe "semaphore"
                    [ test "shows disk horizontal and distant arm inclined for distant signal - Vr2" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr2Semaphore }
                    , describe "distant proceed"
                        [ test "shows both main arms inclined and distant disk flipped away - Hp2/Vr1" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp2Semaphore, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                        ]
                    , test "shows both arms inclined for main signal - Hp2" <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp2Semaphore, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "without slow speed capability and speed limit below 70 km/h"
            (let
                messages =
                    [ SetAspect Proceed, SetSpeedLimit (Just 6) ]
             in
                [ describe "light signal"
                    [ test "shows two green lights for distant - Vr1" <|
                        expectedLightsAfterAppliedToDistant messages
                            { hp = Nothing, vr = vr1Light }
                    , test "shows two green lights and Kennlicht for repeater - Vr1" <|
                        expectedLightsAfterAppliedToRepeater messages
                            { hp = Nothing, vr = vr1LightKennlicht }
                    , describe "distant proceed"
                        [ test "shows green main light and two green distant lights - Hp1/Vr1" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp1LightNoOrangeLight, vr = vr1Light }
                        ]
                    , describe "distant proceed with speed limit"
                        [ test "shows green main light and green/orange distant lights - Hp1/Vr2" <|
                            expectedLightsAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
                                messages
                                { hp = hp1LightNoOrangeLight, vr = vr2Light }
                        ]
                    , test "shows green light for main signal - Hp1" <|
                        expectedLightsAfterAppliedToMain messages
                            { hp = hp1LightNoOrangeLight, vr = Nothing }
                    ]
                , describe "semaphore"
                    [ test "shows flipped away disk for distant - Vr1" <|
                        expectedSemaphoreAfterAppliedToDistant messages
                            { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                    , describe "distant proceed with speed limit"
                        [ test "shows inclined main arm, disk horizontal and distant arm inclined - Hp1/Vr2" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed, ToggleSlowSpeedLight 4, SetSpeedLimit (Just 4) ]
                                messages
                                { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = vr2Semaphore }
                        ]
                    , test "shows inclined main arm - Vr1" <|
                        expectedSemaphoreAfterAppliedToMain messages
                            { hp = hp1SemaphoreNoLowerArm, sh = Arm.Absent, vr = Nothing }
                    ]
                ]
            )
        , describe "Sh1/Ra12"
            [ describe "Stop"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect Stop ]
                 in
                    [ describe "light signal"
                        [ test "shows two orange lights for distant - Vr0" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr0Light }
                        , describe "distant proceed"
                            [ test "shows two main red lights and all distant lights off - Hp0" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp = hp0LightBig, vr = vrLightAllOff }
                            ]
                        , test "shows two red lights for main signal - Hp0" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp = hp0LightBig, vr = Nothing }
                        ]
                    , describe "semaphore"
                        [ test "shows vertical disk for distant - Vr0" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        , test "shows horizontal top arm, horizontal sh and vertical disk for combination - Hp0/Vr0/Sh0" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Stop ]
                                messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Horizontal, vr = vr0SemaphoreNoArm }
                        , test "shows horizontal top arm and horizontal sh - Hp0/Sh0" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Horizontal, vr = Nothing }
                        ]
                    ]
                )
            , describe "StopAndRa12"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect StopAndRa12 ]
                 in
                    [ describe "light signal"
                        [ test "shows two orange lights for distant - Vr0" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr0Light }
                        , describe "distant proceed"
                            [ test "shows one main red light, two white lights and all distant lights off - Hp0/Sh1" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp =
                                        Just
                                            { greenLight = Lamp.Off
                                            , redLight = Lamp.On
                                            , secondaryRedLight = Lamp.Off
                                            , ra12Lights = Lamp.On
                                            , orangeLight = Lamp.Off
                                            , zs1Lights = Lamp.Absent
                                            , zs7Lights = Lamp.Absent
                                            }
                                    , vr = vrLightAllOff
                                    }
                            ]
                        , test "shows one red light and two white lights for main signal - Hp0/Sh1" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Off
                                        , ra12Lights = Lamp.On
                                        , orangeLight = Lamp.Off
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr = Nothing
                                }
                        ]
                    , describe "semaphore"
                        [ test "shows vertical disk for distant - Vr0" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        , test "shows horizontal top arm, inclined sh and vertical disk for combination - Hp0/Vr0/Sh1" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Stop ]
                                messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Inclined, vr = vr0SemaphoreNoArm }
                        , test "shows horizontal top arm and inclined sh - Hp0/Sh1" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp = hp0SemaphoreNoLowerArm, sh = Arm.Inclined, vr = Nothing }
                        ]
                    ]
                )
            , describe "Proceed"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect Proceed ]
                 in
                    [ describe "light signal"
                        [ test "shows two green lights for distant - Vr1" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr1Light }
                        , describe "distant proceed"
                            [ test "shows main green light and two green distant lights - Hp1/Vr1" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp = hp1LightBig, vr = vr1Light }
                            ]
                        , test "shows green light for main signal - Hp1" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp = hp1LightBig, vr = Nothing }
                        ]
                    , describe "semaphore"
                        [ test "shows flipped away disk for distant - Vr1" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                        , test "shows inclined top arm, inclined sh and flipped away disk for combination - Hp1/Vr1/Sh1" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp1SemaphoreNoLowerArm, sh = Arm.Inclined, vr = vr1SemaphoreNoArm }
                        , test "shows inclined top arm and inclined sh - Hp1/Sh1" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp = hp1SemaphoreNoLowerArm, sh = Arm.Inclined, vr = Nothing }
                        ]
                    ]
                )
            , describe "Proceed with speed limit below 70 km/h"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect Proceed, SetSpeedLimit (Just 6) ]
                 in
                    [ describe "light signal"
                        [ test "shows green and orange lights for distant - Vr2" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr2Light }
                        , describe "distant proceed"
                            [ test "shows main green and orange lights and two green distant lights - Hp2/Vr1" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp = hp2LightBig, vr = vr1Light }
                            ]
                        , test "shows green and orange lights for main signal - Hp2" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp = hp2LightBig, vr = Nothing }
                        ]
                    , describe "semaphore"
                        [ test "shows flipped away disk for distant - Vr1" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr1SemaphoreNoArm }
                        , test "shows inclined top arm, inclined sh and flipped away disk for combination - Hp1/Vr1/Sh1" <|
                            expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                messages
                                { hp = hp1SemaphoreNoLowerArm, sh = Arm.Inclined, vr = vr1SemaphoreNoArm }
                        , test "shows inclined top arm and inclined sh - Hp1/Sh1" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp = hp1SemaphoreNoLowerArm, sh = Arm.Inclined, vr = Nothing }
                        ]
                    ]
                )
            ]
        , describe "short break path"
            [ describe "Stop"
                (let
                    messages =
                        [ SetAspect Stop, ToggleShortBrakePath ]
                 in
                    [ describe "distant signal"
                        [ test "shows two orange lights and Kennlicht for light signal - Vr0" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr0LightKennlicht }
                        , test "shows vertical disk and short break path sign - Vr0" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing
                                , sh = Arm.Absent
                                , vr = Just { disk = Arm.Vertical, arm = Arm.Absent, rightLights = Arm.Vertical, shortBreakPathSign = True }
                                }
                        ]
                    , describe "combination signal"
                        [ describe "main stop"
                            [ test "shows main red and distant completely off for light signal - Hp0" <|
                                expectedLightsAfterAppliedToCombination messages
                                    [ SetAspect Stop ]
                                    { hp = hp0LightNoOrangeLight
                                    , vr =
                                        Just
                                            { topOrangeLight = Lamp.Off
                                            , topGreenLight = Lamp.Off
                                            , whiteLight = Lamp.Off
                                            , bottomOrangeLight = Lamp.Off
                                            , bottomGreenLight = Lamp.Off
                                            }
                                    }
                            , test "shows horizontal top arm and orange disk vertical plus short break path sign - Hp0/Vr0" <|
                                expectedSemaphoreAfterAppliedToCombination messages
                                    [ SetAspect Stop ]
                                    { hp = hp0SemaphoreNoLowerArm
                                    , sh = Arm.Absent
                                    , vr = Just { disk = Arm.Vertical, arm = Arm.Absent, rightLights = Arm.Vertical, shortBreakPathSign = True }
                                    }
                            ]
                        , describe "main proceed"
                            [ test "shows main green light and two orange lights plus Kennlicht for light signal - Hp1/Vr0" <|
                                expectedLightsAfterAppliedToCombination messages
                                    [ SetAspect Proceed ]
                                    { hp = hp1LightNoOrangeLight, vr = vr0LightKennlicht }
                            , test "shows inclined top arm and orange disk vertical plus short break path sign - Hp1/Vr0" <|
                                expectedSemaphoreAfterAppliedToCombination messages
                                    [ SetAspect Proceed ]
                                    { hp = hp1SemaphoreNoLowerArm
                                    , sh = Arm.Absent
                                    , vr = Just { disk = Arm.Vertical, arm = Arm.Absent, rightLights = Arm.Vertical, shortBreakPathSign = True }
                                    }
                            ]
                        ]
                    ]
                )
            , describe "Proceed"
                (let
                    messages =
                        [ SetAspect Proceed, ToggleShortBrakePath ]
                 in
                    [ describe "light signal"
                        [ test "shows two green lights and Kennlicht for distant signal - Vr1" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr1LightKennlicht }
                        ]
                    , describe "main proceed"
                        [ test "shows main green light and two green lights plus Kennlicht for combination - Hp1/Vr1" <|
                            expectedLightsAfterAppliedToCombination messages
                                [ SetAspect Proceed ]
                                { hp = hp1LightNoOrangeLight, vr = vr1LightKennlicht }
                        ]
                    ]
                )
            ]
        , describe "Zs7"
            [ describe "Stop"
                (let
                    messages =
                        [ ToggleHasZs7, SetAspect Stop ]
                 in
                    [ describe "light signal"
                        [ test "shows two orange lights for distant - Vr0" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr0Light }
                        , describe "distant proceed"
                            [ test "shows main red light and distant completely off for combination - Hp0" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp =
                                        Just
                                            { greenLight = Lamp.Off
                                            , redLight = Lamp.On
                                            , secondaryRedLight = Lamp.Absent
                                            , ra12Lights = Lamp.Absent
                                            , orangeLight = Lamp.Absent
                                            , zs1Lights = Lamp.Absent
                                            , zs7Lights = Lamp.Off
                                            }
                                    , vr = vrLightAllOff
                                    }
                            ]
                        , test "shows red light for main signal - Hp0" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Off
                                        }
                                , vr = Nothing
                                }
                        ]
                    , describe "semaphore"
                        [ test "shows vertical orange disk for distant - Vr0" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        , describe "distant proceed"
                            [ test "shows horizontal main arm and orange disk vertical - Hp0/Vr0" <|
                                expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp =
                                        Just
                                            { topArm = Arm.Horizontal
                                            , lowerArm = Arm.Absent
                                            , zs1Lights = Lamp.Absent
                                            , zs7Lights = Lamp.Off
                                            }
                                    , sh = Arm.Absent
                                    , vr = vr0SemaphoreNoArm
                                    }
                            ]
                        , test "shows red light for main signal - Hp0" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp =
                                    Just
                                        { topArm = Arm.Horizontal
                                        , lowerArm = Arm.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Off
                                        }
                                , sh = Arm.Absent
                                , vr = Nothing
                                }
                        ]
                    ]
                )
            , describe "StopAndZs7"
                (let
                    messages =
                        [ ToggleHasZs7, SetAspect StopAndZs7 ]
                 in
                    [ describe "light signal"
                        [ test "shows two orange lights for distant - Vr0" <|
                            expectedLightsAfterAppliedToDistant messages
                                { hp = Nothing, vr = vr0Light }
                        , describe "distant proceed"
                            [ test "shows main red light plus Zs7 lights and distant completely off for combination - Hp0" <|
                                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp =
                                        Just
                                            { greenLight = Lamp.Off
                                            , redLight = Lamp.On
                                            , secondaryRedLight = Lamp.Absent
                                            , ra12Lights = Lamp.Absent
                                            , orangeLight = Lamp.Absent
                                            , zs1Lights = Lamp.Absent
                                            , zs7Lights = Lamp.On
                                            }
                                    , vr = vrLightAllOff
                                    }
                            ]
                        , test "shows red light plus Zs7 light for main signal - Hp0" <|
                            expectedLightsAfterAppliedToMain messages
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.On
                                        }
                                , vr = Nothing
                                }
                        ]
                    , describe "semaphore"
                        [ test "shows vertical orange disk for distant - Vr0" <|
                            expectedSemaphoreAfterAppliedToDistant messages
                                { hp = Nothing, sh = Arm.Absent, vr = vr0SemaphoreNoArm }
                        , describe "distant proceed"
                            [ test "shows horizontal main arm plus Zs7 lights and orange disk vertical - Hp0/Vr0" <|
                                expectedSemaphoreAfterAppliedToCombination [ SetAspect Proceed ]
                                    messages
                                    { hp =
                                        Just
                                            { topArm = Arm.Horizontal
                                            , lowerArm = Arm.Absent
                                            , zs1Lights = Lamp.Absent
                                            , zs7Lights = Lamp.On
                                            }
                                    , sh = Arm.Absent
                                    , vr = vr0SemaphoreNoArm
                                    }
                            ]
                        , test "shows red light plus Zs7 lights for main signal - Hp0" <|
                            expectedSemaphoreAfterAppliedToMain messages
                                { hp =
                                    Just
                                        { topArm = Arm.Horizontal
                                        , lowerArm = Arm.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.On
                                        }
                                , sh = Arm.Absent
                                , vr = Nothing
                                }
                        ]
                    ]
                )
            ]
        ]


hp0LightNoOrangeLight =
    Just
        { greenLight = Lamp.Off
        , redLight = Lamp.On
        , secondaryRedLight = Lamp.Absent
        , ra12Lights = Lamp.Absent
        , orangeLight = Lamp.Absent
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp0LightWithOrangeLight =
    Just
        { greenLight = Lamp.Off
        , redLight = Lamp.On
        , secondaryRedLight = Lamp.Absent
        , ra12Lights = Lamp.Absent
        , orangeLight = Lamp.Off
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp0LightBig =
    Just
        { greenLight = Lamp.Off
        , redLight = Lamp.On
        , secondaryRedLight = Lamp.On
        , ra12Lights = Lamp.Off
        , orangeLight = Lamp.Off
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp1LightNoOrangeLight =
    Just
        { greenLight = Lamp.On
        , redLight = Lamp.Off
        , secondaryRedLight = Lamp.Absent
        , ra12Lights = Lamp.Absent
        , orangeLight = Lamp.Absent
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp1LightWithOrangeLight =
    Just
        { greenLight = Lamp.On
        , redLight = Lamp.Off
        , secondaryRedLight = Lamp.Absent
        , ra12Lights = Lamp.Absent
        , orangeLight = Lamp.Off
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp1LightBig =
    Just
        { greenLight = Lamp.On
        , redLight = Lamp.Off
        , secondaryRedLight = Lamp.Off
        , ra12Lights = Lamp.Off
        , orangeLight = Lamp.Off
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp2Light =
    Just
        { greenLight = Lamp.On
        , redLight = Lamp.Off
        , secondaryRedLight = Lamp.Absent
        , ra12Lights = Lamp.Absent
        , orangeLight = Lamp.On
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp2LightBig =
    Just
        { greenLight = Lamp.On
        , redLight = Lamp.Off
        , secondaryRedLight = Lamp.Off
        , ra12Lights = Lamp.Off
        , orangeLight = Lamp.On
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


vr0Light =
    Just
        { topOrangeLight = Lamp.On
        , topGreenLight = Lamp.Off
        , whiteLight = Lamp.Absent
        , bottomOrangeLight = Lamp.On
        , bottomGreenLight = Lamp.Off
        }


vr0LightKennlicht =
    Just
        { topOrangeLight = Lamp.On
        , topGreenLight = Lamp.Off
        , whiteLight = Lamp.On
        , bottomOrangeLight = Lamp.On
        , bottomGreenLight = Lamp.Off
        }


vr1Light =
    Just
        { topOrangeLight = Lamp.Off
        , topGreenLight = Lamp.On
        , whiteLight = Lamp.Absent
        , bottomOrangeLight = Lamp.Off
        , bottomGreenLight = Lamp.On
        }


vr1LightKennlicht =
    Just
        { topOrangeLight = Lamp.Off
        , topGreenLight = Lamp.On
        , whiteLight = Lamp.On
        , bottomOrangeLight = Lamp.Off
        , bottomGreenLight = Lamp.On
        }


vr2Light =
    Just
        { topOrangeLight = Lamp.Off
        , topGreenLight = Lamp.On
        , whiteLight = Lamp.Absent
        , bottomOrangeLight = Lamp.On
        , bottomGreenLight = Lamp.Off
        }


vr2LightKennlicht =
    Just
        { topOrangeLight = Lamp.Off
        , topGreenLight = Lamp.On
        , whiteLight = Lamp.On
        , bottomOrangeLight = Lamp.On
        , bottomGreenLight = Lamp.Off
        }


vrLightAllOff =
    Just
        { topOrangeLight = Lamp.Off
        , topGreenLight = Lamp.Off
        , whiteLight = Lamp.Absent
        , bottomOrangeLight = Lamp.Off
        , bottomGreenLight = Lamp.Off
        }


hp0SemaphoreWithLowerArm =
    Just
        { topArm = Arm.Horizontal
        , lowerArm = Arm.Vertical
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp0SemaphoreNoLowerArm =
    Just
        { topArm = Arm.Horizontal
        , lowerArm = Arm.Absent
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp1SemaphoreWithLowerArm =
    Just
        { topArm = Arm.Inclined
        , lowerArm = Arm.Vertical
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp1SemaphoreNoLowerArm =
    Just
        { topArm = Arm.Inclined
        , lowerArm = Arm.Absent
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


hp2Semaphore =
    Just
        { topArm = Arm.Inclined
        , lowerArm = Arm.Inclined
        , zs1Lights = Lamp.Absent
        , zs7Lights = Lamp.Absent
        }


vr0SemaphoreWithArm =
    Just
        { disk = Arm.Vertical
        , arm = Arm.Vertical
        , rightLights = Arm.Vertical
        , shortBreakPathSign = False
        }


vr0SemaphoreNoArm =
    Just
        { disk = Arm.Vertical
        , arm = Arm.Absent
        , rightLights = Arm.Vertical
        , shortBreakPathSign = False
        }


vr1SemaphoreWithArm =
    Just
        { disk = Arm.Horizontal
        , arm = Arm.Vertical
        , rightLights = Arm.Inclined
        , shortBreakPathSign = False
        }


vr1SemaphoreNoArm =
    Just
        { disk = Arm.Horizontal
        , arm = Arm.Absent
        , rightLights = Arm.Inclined
        , shortBreakPathSign = False
        }


vr2Semaphore =
    Just
        { disk = Arm.Vertical
        , arm = Arm.Inclined
        , rightLights = Arm.Inclined
        , shortBreakPathSign = False
        }


expectedLightsAfterAppliedTo : SignalModel.Model -> List Target -> HvLightSignal.Model -> () -> Expect.Expectation
expectedLightsAfterAppliedTo signalModel messages expectedLights =
    \() ->
        Expect.equal
            (List.foldl Signal.update signalModel messages
                |> HvLightSignal.lights
            )
            expectedLights


expectedLightsAfterAppliedToDistant : List Msg -> HvLightSignal.Model -> () -> Expect.Expectation
expectedLightsAfterAppliedToDistant messages expectedLights =
    expectedLightsAfterAppliedTo SignalModel.distantSignal (List.map ToDistantSignal messages) expectedLights


expectedLightsAfterAppliedToRepeater : List Msg -> HvLightSignal.Model -> () -> Expect.Expectation
expectedLightsAfterAppliedToRepeater messages expectedLights =
    expectedLightsAfterAppliedTo SignalModel.signalRepeater (List.map ToDistantSignal messages) expectedLights


expectedLightsAfterAppliedToCombination : List Msg -> List Msg -> HvLightSignal.Model -> () -> Expect.Expectation
expectedLightsAfterAppliedToCombination distantMessages mainMessages expectedLights =
    let
        messages =
            List.append
                (List.map ToDistantSignal distantMessages)
                (List.map ToMainSignal mainMessages)
    in
        expectedLightsAfterAppliedTo SignalModel.combinationSignal messages expectedLights


expectedLightsAfterAppliedToMain : List Msg -> HvLightSignal.Model -> () -> Expect.Expectation
expectedLightsAfterAppliedToMain messages expectedLights =
    expectedLightsAfterAppliedTo SignalModel.mainSignal (List.map ToMainSignal messages) expectedLights


expectedSemaphoreAfterAppliedTo : SignalModel.Model -> List Target -> HvSemaphore.Model -> () -> Expect.Expectation
expectedSemaphoreAfterAppliedTo signalModel messages expectedArms =
    \() ->
        Expect.equal
            (List.foldl Signal.update signalModel messages
                |> HvSemaphore.arms
            )
            expectedArms


expectedSemaphoreAfterAppliedToDistant : List Msg -> HvSemaphore.Model -> () -> Expect.Expectation
expectedSemaphoreAfterAppliedToDistant messages expectedArms =
    expectedSemaphoreAfterAppliedTo SignalModel.distantSignal (List.map ToDistantSignal messages) expectedArms


expectedSemaphoreAfterAppliedToCombination : List Msg -> List Msg -> HvSemaphore.Model -> () -> Expect.Expectation
expectedSemaphoreAfterAppliedToCombination distantMessages mainMessages expectedArms =
    let
        messages =
            List.append
                (List.map ToDistantSignal distantMessages)
                (List.map ToMainSignal mainMessages)
    in
        expectedSemaphoreAfterAppliedTo SignalModel.combinationSignal messages expectedArms


expectedSemaphoreAfterAppliedToMain : List Msg -> HvSemaphore.Model -> () -> Expect.Expectation
expectedSemaphoreAfterAppliedToMain messages expectedArms =
    expectedSemaphoreAfterAppliedTo SignalModel.mainSignal (List.map ToMainSignal messages) expectedArms
