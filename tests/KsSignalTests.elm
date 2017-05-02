module KsSignalTests exposing (..)

import Expect
import KsSignal
import Lamp
import Messages exposing (..)
import Signal
import SignalModel
import Test exposing (..)


all : Test
all =
    describe "Signal"
        [ describe ".ksSignal"
            [ describe "DistantSignal"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal Stop)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.On
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal Proceed)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed with Zs3, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed with Zs3 and limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetZs3SpeedLimit (Just 5)))
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.Blinking
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , describe "shortBrakePath"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.distantSignal
                                    |> Signal.update (ToDistantSignal Stop)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.On
                                , redLight = Lamp.Absent
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.On
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "Expect Proceed" <|
                        \() ->
                            Expect.equal
                                (SignalModel.distantSignal
                                    |> Signal.update (ToDistantSignal Proceed)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Off
                                , redLight = Lamp.Absent
                                , greenLight = Lamp.On
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "Expect Proceed with Zs3 and limit" <|
                        \() ->
                            Expect.equal
                                (SignalModel.distantSignal
                                    |> Signal.update (ToDistantSignal Proceed)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                    |> Signal.update (ToDistantSignal (SetZs3SpeedLimit (Just 5)))
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.On
                                , redLight = Lamp.Absent
                                , greenLight = Lamp.Blinking
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    ]
                ]
            , describe "DistantSignal repeater"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal Stop)
                                |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.On
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.On
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal Proceed)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Off
                            }
                , test "Expect Proceed with Zs3, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Off
                            }
                , test "Expect Proceed with Zs3 and limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.signalRepeater
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetZs3SpeedLimit (Just 5)))
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Absent
                            , greenLight = Lamp.Blinking
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.On
                            }
                ]
            , describe "CombinationSignal"
                [ test "Stop (distant Stop)" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Stop)
                                |> Signal.update (ToDistantSignal Stop)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.On
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Stop (distant Proceed)" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Stop)
                                |> Signal.update (ToDistantSignal Proceed)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.On
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Proceed)
                                |> Signal.update (ToDistantSignal Stop)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Off
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.On
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Proceed)
                                |> Signal.update (ToDistantSignal Proceed)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Off
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed with Zs3, but no limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Proceed)
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Off
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Expect Proceed with Zs3 and limit" <|
                    \() ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToMainSignal Proceed)
                                |> Signal.update (ToDistantSignal Proceed)
                                |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                |> Signal.update (ToDistantSignal (SetZs3SpeedLimit (Just 5)))
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Off
                            , greenLight = Lamp.Blinking
                            , orangeLight = Lamp.Off
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , describe "Ra12"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Off
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Off
                                }
                    , test "StopAndRa12" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal StopAndRa12)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.On
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.On
                                }
                    ]
                , describe "shortBrakePath"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal Stop)
                                    |> Signal.update (ToDistantSignal Stop)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Off
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "Expect Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal Proceed)
                                    |> Signal.update (ToDistantSignal Stop)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.On
                                , redLight = Lamp.Off
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.On
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "Expect Proceed" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal Proceed)
                                    |> Signal.update (ToDistantSignal Proceed)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Off
                                , redLight = Lamp.Off
                                , greenLight = Lamp.On
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "Expect Proceed with Zs3 and limit" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal Proceed)
                                    |> Signal.update (ToDistantSignal Proceed)
                                    |> Signal.update (ToDistantSignal ToggleShortBrakePath)
                                    |> Signal.update (ToDistantSignal SetZs3Dynamic)
                                    |> Signal.update (ToDistantSignal (SetZs3SpeedLimit (Just 5)))
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.On
                                , redLight = Lamp.Off
                                , greenLight = Lamp.Blinking
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    ]
                , describe "Zs1"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Off
                                }
                    , test "StopAndZs1" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal StopAndZs1)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Blinking
                                }
                    ]
                , describe "Zs7"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Off
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "StopAndZs7" <|
                        \() ->
                            Expect.equal
                                (SignalModel.combinationSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal StopAndZs7)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Off
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.On
                                , bottomWhiteLight = Lamp.Absent
                                }
                    ]
                ]
            , describe "MainSignal"
                [ test "Stop" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal Stop)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.On
                            , greenLight = Lamp.Off
                            , orangeLight = Lamp.Absent
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , test "Proceed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal Proceed)
                                |> KsSignal.lights
                            )
                            { topWhiteLight = Lamp.Absent
                            , redLight = Lamp.Off
                            , greenLight = Lamp.On
                            , orangeLight = Lamp.Absent
                            , centerWhiteLight = Lamp.Absent
                            , zs7Lights = Lamp.Absent
                            , bottomWhiteLight = Lamp.Absent
                            }
                , describe "Ra12"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.Off
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Off
                                }
                    , test "StopAndRa12" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasRa12)
                                    |> Signal.update (ToMainSignal StopAndRa12)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.On
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.On
                                }
                    ]
                , describe "Zs1"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.Off
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "StopAndZs1" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs1)
                                    |> Signal.update (ToMainSignal StopAndZs1)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.Blinking
                                , zs7Lights = Lamp.Absent
                                , bottomWhiteLight = Lamp.Absent
                                }
                    ]
                , describe "Zs7"
                    [ test "Stop" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal Stop)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.Off
                                , bottomWhiteLight = Lamp.Absent
                                }
                    , test "StopAndZs7" <|
                        \() ->
                            Expect.equal
                                (SignalModel.mainSignal
                                    |> Signal.update (ToMainSignal ToggleHasZs7)
                                    |> Signal.update (ToMainSignal StopAndZs7)
                                    |> KsSignal.lights
                                )
                                { topWhiteLight = Lamp.Absent
                                , redLight = Lamp.On
                                , greenLight = Lamp.Off
                                , orangeLight = Lamp.Absent
                                , centerWhiteLight = Lamp.Absent
                                , zs7Lights = Lamp.On
                                , bottomWhiteLight = Lamp.Absent
                                }
                    ]
                ]
            ]
        ]
