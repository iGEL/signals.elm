module HvSignalTests exposing (..)

import Expect
import HvSignal
import Lamp
import Messages exposing (..)
import Signal
import SignalModel
import Test exposing (..)


all : Test
all =
    describe "Signal"
        [ describe ".hvSignal"
            [ describe "DistantSignal"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Stop))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit above 60 km/h" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 7)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h when has proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h when main signal has Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal ToggleHasRa12)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h when hasn't proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , describe "shortBrakePath"
                    [ test "Expect Proceed" <|
                        \() ->
                            Expect.equal
                                (SignalModel.distantSignal
                                    |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> HvSignal.lights
                                )
                                { hp = Nothing
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.On
                                        , whiteLight = Lamp.On
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.On
                                        }
                                }
                    ]
                ]
            , describe "DistantSignal repeater"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal (SetAspect Stop))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.On
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.On
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 kmph with proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.On
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h when main signal has Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal ToggleHasRa12)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.On
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 kmph without proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp = Nothing
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.On
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                ]
            , describe "CombinationSignal"
                [ test "Stop (distant Stop)" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Stop))
                                |> Signal.update (ToDistantSignal (SetAspect Stop))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.Off
                                    , redLight = Lamp.On
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Stop (distant Proceed)" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Stop))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.Off
                                    , redLight = Lamp.On
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Stop))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit above 60 km/h" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 7)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h with proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h when next main signal has Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal ToggleHasRa12)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Expect Proceed with Zs3v and limit below 70 km/h without proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.Off
                                    , topGreenLight = Lamp.On
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.Off
                                    , bottomGreenLight = Lamp.On
                                    }
                            }
                , test "Proceed with Zs3 and limit above 60 km/h" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 7)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Off
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Proceed with Zs3 and limit below 70 km/h with proceed slowly lamp" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.On
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Proceed with Zs3 and limit below 70 km/h without proceed slowly lamp" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , test "Proceed with Zs3 and limit below 70 km/h with Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal ToggleHasRa12)
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Off
                                    , ra12Lights = Lamp.Off
                                    , orangeLight = Lamp.On
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr =
                                Just
                                    { topOrangeLight = Lamp.On
                                    , topGreenLight = Lamp.Off
                                    , whiteLight = Lamp.Absent
                                    , bottomOrangeLight = Lamp.On
                                    , bottomGreenLight = Lamp.Off
                                    }
                            }
                , describe "Ra12"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.On
                                        , ra12Lights = Lamp.Off
                                        , orangeLight = Lamp.Off
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    , test "StopAndRa12" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndRa12))
                                    |> HvSignal.lights
                                )
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
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    ]
                , describe "shortBrakePath"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> Signal.update (ToDistantSignal (SetAspect Stop))
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Off
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    , test "Expect Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal (SetAspect Proceed))
                                    |> Signal.update (ToDistantSignal (SetAspect Stop))
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.On
                                        , redLight = Lamp.Off
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.On
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.On
                                        , bottomOrangeLight = Lamp.On
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    , test "Expect Proceed" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal (SetAspect Proceed))
                                    |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.On
                                        , redLight = Lamp.Off
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.On
                                        , whiteLight = Lamp.On
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.On
                                        }
                                }
                    ]
                , describe "Zs1"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Off
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    , test "StopAndZs1" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndZs1))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.On
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    ]
                , describe "Zs7"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToDistantSignal (SetAspect Proceed))
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
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
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    , test "StopAndZs7" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndZs7))
                                    |> HvSignal.lights
                                )
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
                                , vr =
                                    Just
                                        { topOrangeLight = Lamp.Off
                                        , topGreenLight = Lamp.Off
                                        , whiteLight = Lamp.Absent
                                        , bottomOrangeLight = Lamp.Off
                                        , bottomGreenLight = Lamp.Off
                                        }
                                }
                    ]
                ]
            , describe "MainSignal"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Stop))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.Off
                                    , redLight = Lamp.On
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed with Zs3, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Off
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed with Zs3 and limit above 60 km/h" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 7)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Off
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed with Zs3 and limit below 70 km/h with proceed slowly lamp" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.On
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed with Zs3 and limit below 70 km/h without proceed slowly lamp" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Absent
                                    , ra12Lights = Lamp.Absent
                                    , orangeLight = Lamp.Absent
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , test "Proceed with Zs3 and limit below 70 km/h with Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetAspect Proceed))
                                |> Signal.update (ToMainSignal ToggleHasRa12)
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 6)))
                                |> HvSignal.lights
                            )
                            { hp =
                                Just
                                    { greenLight = Lamp.On
                                    , redLight = Lamp.Off
                                    , secondaryRedLight = Lamp.Off
                                    , ra12Lights = Lamp.Off
                                    , orangeLight = Lamp.On
                                    , zs1Lights = Lamp.Absent
                                    , zs7Lights = Lamp.Absent
                                    }
                            , vr = Nothing
                            }
                , describe "Ra12"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.On
                                        , ra12Lights = Lamp.Off
                                        , orangeLight = Lamp.Off
                                        , zs1Lights = Lamp.Absent
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr = Nothing
                                }
                    , test "StopAndRa12" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndRa12))
                                    |> HvSignal.lights
                                )
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
                , describe "Zs1"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.Off
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr = Nothing
                                }
                    , test "StopAndZs1" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndZs1))
                                    |> HvSignal.lights
                                )
                                { hp =
                                    Just
                                        { greenLight = Lamp.Off
                                        , redLight = Lamp.On
                                        , secondaryRedLight = Lamp.Absent
                                        , ra12Lights = Lamp.Absent
                                        , orangeLight = Lamp.Absent
                                        , zs1Lights = Lamp.On
                                        , zs7Lights = Lamp.Absent
                                        }
                                , vr = Nothing
                                }
                    ]
                , describe "Zs7"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal (SetAspect Stop))
                                    |> HvSignal.lights
                                )
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
                    , test "StopAndZs7" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal (SetAspect StopAndZs7))
                                    |> HvSignal.lights
                                )
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
                ]
            ]
        ]
