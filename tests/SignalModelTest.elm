module SignalModelTest exposing (..)

import Expect
import Test exposing (..)
import SignalModel
import Signal
import Messages exposing (..)


all : Test
all =
    describe "SignalModel"
        [ describe ".mainSignalSpeedLimit"
            [ describe "DistantSignal"
                [ test "is always Nothing" <|
                    \_ ->
                        Expect.equal
                            (SignalModel.distantSignal
                                |> SignalModel.mainSignalSpeedLimit
                            )
                            Nothing
                ]
            , describe "CombinationSignal"
                [ test "returns the speed limit of the main signal" <|
                    \_ ->
                        Expect.equal
                            (SignalModel.combinationSignal
                                |> Signal.update (ToDistantSignal (SetSpeedLimit (Just 2)))
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 8)))
                                |> SignalModel.mainSignalSpeedLimit
                            )
                            (Just 8)
                ]
            , describe "MainSignal"
                [ test "returns the speed limit" <|
                    \_ ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (SetSpeedLimit (Just 5)))
                                |> SignalModel.mainSignalSpeedLimit
                            )
                            (Just 5)
                ]
            ]
        , describe ".availableSpeedLimits"
            [ describe "KsSignal"
                [ test "without Zs3" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> SignalModel.availableSpeedLimits SignalModel.Ks
                            )
                            [ Nothing ]
                , test "with dynamic Zs3" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> SignalModel.availableSpeedLimits SignalModel.Ks
                            )
                            [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 10, Just 11, Just 12, Just 13, Just 14, Just 15, Nothing ]
                , test "with fixed Zs3" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal SetZs3Fixed)
                                |> SignalModel.availableSpeedLimits SignalModel.Ks
                            )
                            [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 10, Just 11, Just 12, Just 13, Just 14, Just 15 ]
                ]
            , describe "HvSignal"
                [ test "without Zs3, proceed slowly or Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> SignalModel.availableSpeedLimits SignalModel.HvLight
                            )
                            [ Nothing ]
                , test "with proceed slowly" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 4))
                                |> SignalModel.availableSpeedLimits SignalModel.HvLight
                            )
                            [ Just 4, Nothing ]
                , test "with Ra12" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal ToggleHasRa12)
                                |> SignalModel.availableSpeedLimits SignalModel.HvLight
                            )
                            [ Just 4, Nothing ]
                , test "with dynamic Zs3" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal SetZs3Dynamic)
                                |> SignalModel.availableSpeedLimits SignalModel.HvLight
                            )
                            [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 10, Just 11, Just 12, Just 13, Just 14, Just 15, Nothing ]
                , test "with fixed Zs3" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal SetZs3Fixed)
                                |> SignalModel.availableSpeedLimits SignalModel.HvLight
                            )
                            [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 10, Just 11, Just 12, Just 13, Just 14, Just 15 ]
                ]
            , describe "HlSignal"
                [ test "without stripes" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> SignalModel.availableSpeedLimits SignalModel.Hl
                            )
                            [ Just 4, Nothing ]
                , test "with orange stripe" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 6))
                                |> SignalModel.availableSpeedLimits SignalModel.Hl
                            )
                            [ Just 4, Just 6, Nothing ]
                , test "with green stripe" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 10))
                                |> SignalModel.availableSpeedLimits SignalModel.Hl
                            )
                            [ Just 4, Just 10, Nothing ]
                , test "with both stripes" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 6))
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 10))
                                |> SignalModel.availableSpeedLimits SignalModel.Hl
                            )
                            [ Just 4, Just 6, Just 10, Nothing ]
                , test "with an unrepresentable speed allowed" <|
                    \() ->
                        Expect.equal
                            (SignalModel.mainSignal
                                |> Signal.update (ToMainSignal (ToggleSlowSpeedLight 2))
                                |> SignalModel.availableSpeedLimits SignalModel.Hl
                            )
                            [ Just 4, Nothing ]
                ]
            ]
        ]
