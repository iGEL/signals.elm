module HlSignalTests exposing (all)

import Expect
import HlSignal
import Lamp
import Messages exposing (..)
import Signal
import SignalModel
import Test exposing (..)


expectedLightsAfterAppliedToDistant messages expectedLights =
    \() ->
        Expect.equal
            (List.foldl Signal.update SignalModel.distantSignal (List.map ToDistantSignal messages)
                |> HlSignal.lights
            )
            expectedLights


expectedLightsAfterAppliedToCombination distantMessages mainMessages expectedLights =
    \() ->
        let
            messages =
                List.append
                    (List.map ToDistantSignal distantMessages)
                    (List.map ToMainSignal mainMessages)
        in
        Expect.equal
            (List.foldl Signal.update SignalModel.combinationSignal messages
                |> HlSignal.lights
            )
            expectedLights


expectedLightsAfterAppliedToMain messages expectedLights =
    \() ->
        Expect.equal
            (List.foldl Signal.update SignalModel.mainSignal (List.map ToMainSignal messages)
                |> HlSignal.lights
            )
            expectedLights


all : Test
all =
    describe "Hl signal"
        [ describe "Stop"
            (let
                messages =
                    [ SetAspect Stop ]
             in
             [ test "shows top orange for distant signal - Hl 10" <|
                expectedLightsAfterAppliedToDistant messages
                    { topOrangeLight = Lamp.On
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Absent
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Absent
                    , secondaryRedLight = Lamp.Absent
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows top orange for distant part of combination signal (main proceed) - Hl 10" <|
                expectedLightsAfterAppliedToCombination messages
                    [ SetAspect Proceed ]
                    { topOrangeLight = Lamp.On
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows red for main part of combination signal - Hp 0" <|
                expectedLightsAfterAppliedToCombination []
                    messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.On
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows red for main signal - Hp 0" <|
                expectedLightsAfterAppliedToMain messages
                    { topOrangeLight = Lamp.Absent
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.On
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             ]
            )
        , describe "Stop with speed limit"
            (let
                messages =
                    [ SetAspect Stop, ToggleSlowSpeedLight 6, SetSpeedLimit (Just 6) ]
             in
             [ test "shows top orange for distant signal - Hl 10" <|
                expectedLightsAfterAppliedToDistant messages
                    { topOrangeLight = Lamp.On
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Absent
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Absent
                    , secondaryRedLight = Lamp.Absent
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows top orange for distant part of combination signal (main proceed) - Hl 10" <|
                expectedLightsAfterAppliedToCombination messages
                    [ SetAspect Proceed ]
                    { topOrangeLight = Lamp.On
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows red for main part of combination signal - Hp 0" <|
                expectedLightsAfterAppliedToCombination []
                    messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.On
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Off
                    }
             , test "shows red for main signal - Hp 0" <|
                expectedLightsAfterAppliedToMain messages
                    { topOrangeLight = Lamp.Absent
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.On
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Off
                    }
             ]
            )
        , describe "Proceed"
            (let
                messages =
                    [ SetAspect Proceed ]
             in
             [ test "shows green for distant signal - Hl 1" <|
                expectedLightsAfterAppliedToDistant messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Absent
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Absent
                    , secondaryRedLight = Lamp.Absent
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows green for combination signal - Hl 1" <|
                expectedLightsAfterAppliedToCombination messages
                    messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "show green for main signal - Hl 1" <|
                expectedLightsAfterAppliedToMain messages
                    { topOrangeLight = Lamp.Absent
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             ]
            )
        , describe "Proceed with green & orange stripes, but no speed limit"
            (let
                messages =
                    [ SetAspect Proceed, ToggleSlowSpeedLight 10, ToggleSlowSpeedLight 6 ]
             in
             [ test "shows green for distant signal - Hl 1" <|
                expectedLightsAfterAppliedToDistant messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Absent
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Absent
                    , secondaryRedLight = Lamp.Absent
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows green for combination signal (distant) - Hl 1" <|
                expectedLightsAfterAppliedToCombination messages
                    [ SetAspect Proceed ]
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows green for combination signal (main) - Hl 1" <|
                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                    messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Off
                    , orangeStripe = Lamp.Off
                    }
             , test "show green for main signal - Hl 1" <|
                expectedLightsAfterAppliedToMain messages
                    { topOrangeLight = Lamp.Absent
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Off
                    , orangeStripe = Lamp.Off
                    }
             ]
            )
        , describe "Proceed with 40 km/h"
            (let
                messages =
                    [ SetAspect Proceed, SetSpeedLimit (Just 4) ]
             in
             [ test "shows blinking top orange for distant signal - Hl 7" <|
                expectedLightsAfterAppliedToDistant messages
                    { topOrangeLight = Lamp.Blinking
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Absent
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Absent
                    , secondaryRedLight = Lamp.Absent
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows blinking top orange for combination signal (distant) - Hl 7" <|
                expectedLightsAfterAppliedToCombination messages
                    [ SetAspect Proceed ]
                    { topOrangeLight = Lamp.Blinking
                    , greenLight = Lamp.Off
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.Off
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "shows green and bottom orange for combination signal (main) - Hl 3a" <|
                expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                    messages
                    { topOrangeLight = Lamp.Off
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.On
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             , test "show green and bottom orange for main signal - Hl 3a" <|
                expectedLightsAfterAppliedToMain messages
                    { topOrangeLight = Lamp.Absent
                    , greenLight = Lamp.On
                    , topWhiteLight = Lamp.Absent
                    , redLight = Lamp.Off
                    , bottomWhiteLight = Lamp.Absent
                    , bottomOrangeLight = Lamp.On
                    , secondaryRedLight = Lamp.Off
                    , greenStripe = Lamp.Absent
                    , orangeStripe = Lamp.Absent
                    }
             ]
            )
        , describe "Proceed with 60 km/h"
            [ describe "without orange stripe"
                (let
                    messages =
                        [ SetAspect Proceed, SetSpeedLimit (Just 6) ]
                 in
                 [ test "shows green for distant signal - Hl 1" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows green for combination signal - Hl 1" <|
                    expectedLightsAfterAppliedToCombination messages
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "show green for main signal - Hl 1" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            , describe "with orange stripe"
                (let
                    messages =
                        [ SetAspect Proceed, ToggleSlowSpeedLight 6, SetSpeedLimit (Just 6) ]
                 in
                 [ test "shows blinking orange for distant signal - Hl 7" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.Blinking
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows blinking orange for combination signal (distant) - Hl 7" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.Blinking
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows green, bottom orange and orange stripe for combination signal (main) - Hl 3b" <|
                    expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.On
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.On
                        }
                 , test "shows green, bottom orange and green stripe for main signal - Hl 3b" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.On
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.On
                        }
                 ]
                )
            ]
        , describe "Proceed with 100 km/h"
            [ describe "without green stripe"
                (let
                    messages =
                        [ SetAspect Proceed, SetSpeedLimit (Just 10) ]
                 in
                 [ test "shows green for distant signal - Hl 1" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows green for combination signal - Hl 1" <|
                    expectedLightsAfterAppliedToCombination messages
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "show green for main signal - Hl 1" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            , describe "with green stripe"
                (let
                    messages =
                        [ SetAspect Proceed, ToggleSlowSpeedLight 10, SetSpeedLimit (Just 10) ]
                 in
                 [ test "shows blinking green for distant signal - Hl 4" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Blinking
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows blinking green for combination signal (distant) - Hl 4" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Blinking
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows green, bottom orange and green stripe for combination signal (main) - Hl 2" <|
                    expectedLightsAfterAppliedToCombination [ SetAspect Proceed ]
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.On
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.On
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows green, bottom orange and green stripe for main signal - Hl 2" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.On
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.On
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.On
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            ]
        , describe "Zs1"
            [ describe "with stop aspect"
                (let
                    messages =
                        [ ToggleHasZs1, SetAspect Stop ]
                 in
                 [ test "show the normal orange light for distant signal" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows the normal top orange for distant part of combination signal" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows red for main part of combination signal" <|
                    expectedLightsAfterAppliedToCombination []
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Off
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows red for main signal - Hp 0" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Off
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            , describe "with stop & Zs1 aspect"
                (let
                    messages =
                        [ ToggleHasZs1, SetAspect StopAndZs1 ]
                 in
                 [ test "show the normal orange light for distant signal" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows the normal top orange for distant part of combination signal" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows blinking lower white light and red for main part of combination signal" <|
                    expectedLightsAfterAppliedToCombination []
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Blinking
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows blinking lower write light and red for main signal" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Blinking
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            ]
        , describe "Ra12"
            [ describe "with stop aspect"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect Stop ]
                 in
                 [ test "show the normal orange light for distant signal" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows the normal top orange for distant part of combination signal" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows red for main part of combination signal" <|
                    expectedLightsAfterAppliedToCombination []
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Off
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Off
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows red for main signal - Hp 0" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Off
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.Off
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            , describe "with stop & Ra12 aspect"
                (let
                    messages =
                        [ ToggleHasRa12, SetAspect StopAndRa12 ]
                 in
                 [ test "show the normal orange light for distant signal" <|
                    expectedLightsAfterAppliedToDistant messages
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Absent
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Absent
                        , secondaryRedLight = Lamp.Absent
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows the normal top orange for distant part of combination signal" <|
                    expectedLightsAfterAppliedToCombination messages
                        [ SetAspect Proceed ]
                        { topOrangeLight = Lamp.On
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.Absent
                        , redLight = Lamp.Off
                        , bottomWhiteLight = Lamp.Absent
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows both white lights and red for main part of combination signal" <|
                    expectedLightsAfterAppliedToCombination []
                        messages
                        { topOrangeLight = Lamp.Off
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.On
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.On
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 , test "shows both write lights and red for main signal" <|
                    expectedLightsAfterAppliedToMain messages
                        { topOrangeLight = Lamp.Absent
                        , greenLight = Lamp.Off
                        , topWhiteLight = Lamp.On
                        , redLight = Lamp.On
                        , bottomWhiteLight = Lamp.On
                        , bottomOrangeLight = Lamp.Off
                        , secondaryRedLight = Lamp.Off
                        , greenStripe = Lamp.Absent
                        , orangeStripe = Lamp.Absent
                        }
                 ]
                )
            ]
        ]
