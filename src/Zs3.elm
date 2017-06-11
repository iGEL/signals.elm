module Zs3 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Display
import Messages exposing (..)


type Appearance
    = Absent
    | Dynamic
    | Fixed


type Location
    = DistantSignalLocation
    | MainSignalLocation


isAbsent : Appearance -> Bool
isAbsent appearance =
    appearance == Absent


isPresent : Appearance -> Bool
isPresent appearance =
    not (isAbsent appearance)


isDynamic : Appearance -> Bool
isDynamic appearance =
    appearance == Dynamic


isFixed : Appearance -> Bool
isFixed appearance =
    appearance == Fixed


view : Location -> Appearance -> Maybe Int -> Bool -> Svg msg
view location appearance maybeSpeedLimit signalShowsStop =
    case appearance of
        Dynamic ->
            g [ transform "translate(8,19)" ]
                [ let
                    color =
                        case location of
                            MainSignalLocation ->
                                "white"

                            DistantSignalLocation ->
                                "orange"
                  in
                    case maybeSpeedLimit of
                        Just speedLimit ->
                            if signalShowsStop then
                                Display.view color Off
                            else
                                Display.view color (On (toString speedLimit))

                        Nothing ->
                            Display.view color Off
                ]

        Fixed ->
            case maybeSpeedLimit of
                Just speedLimit ->
                    viewFixed location speedLimit

                Nothing ->
                    g [] []

        Absent ->
            g [] []


digitPaths : Array.Array (List { mainHead : String, distantHead : String, rest : String })
digitPaths =
    Array.fromList
        [ -- 1
          [ { mainHead = "m -25.933042,-13.800823"
            , distantHead = "m 22.803334,46.570801"
            , rest = " q -0.27,0 -0.27,-0.27 l 0,-26.1 q -1.035,0.765 -2.025,1.53 -0.99,0.72 -2.025,1.44 -0.135,0.09 -0.27,0.045 -0.135,-0.045 -0.135,-0.225 l 0,-4.77 q 0,-0.135 0.09,-0.225 l 4.545,-3.285 4.59,0 q 0.27,0 0.27,0.27 l 0,31.32 q 0,0.27 -0.27,0.27 l -4.5,0 z"
            }
          ]
          -- 2
        , [ { mainHead = "m -33.048322,-13.890823"
            , distantHead = "m 16.991171,41.951252"
            , rest = " q -0.24,0 -0.24,-0.2 l 0,-3.84 10.92,-13.68 0,0.04 q 0.6,-0.8 0.76,-1.4 0.2,-0.64 0.2,-1.24 0,-1.36 -0.96,-2.56 -0.96,-1.16 -2.64,-1.16 -1.56,0 -2.64,1.04 -0.56,0.48 -0.84,1.2 -0.28,0.68 -0.36,1.56 0,0.24 -0.24,0.24 l -3.96,0 q -0.24,0 -0.24,-0.24 0,-3.48 2.36,-5.84 2.32,-2.4 5.64,-2.4 3.52,0 5.92,2.4 2.4,2.4 2.4,5.8 0,2.84 -1.72,5.04 l 0,-0.04 q -2.28,2.84 -4.36,5.44 -2.04,2.6 -4.32,5.44 l 10.16,0 q 0.24,0 0.24,0.24 l 0,3.96 q 0,0.2 -0.24,0.2 l -15.84,0 z"
            }
          ]
          -- 3
        , [ { mainHead = "m -26.983753,-26.450823"
            , distantHead = "m 23.055748,30.436532"
            , rest = " q -0.24,0 -0.24,-0.24 l 0,-3.48 q 0,-0.24 0.24,-0.24 l 1.36,0 q 1.88,0 3,-1.04 1.08,-1.08 1.08,-2.68 0,-1.6 -1.12,-2.68 -1.12,-1.12 -2.72,-1.12 -1.12,0 -2.24,0.8 -1.08,0.84 -1.36,2.36 0,0.2 -0.2,0.2 l -4,0 q -0.24,0 -0.24,-0.24 0.24,-3.04 2.56,-5.32 2.32,-2.24 5.32,-2.24 3.76,0 6.16,2.52 1.08,1.24 1.68,2.68 0.6,1.44 0.6,2.96 0,1.68 -0.64,3.24 -0.32,0.68 -0.88,1.32 -0.52,0.6 -1.24,1.16 0.8,0.56 1.36,1.24 0.56,0.68 0.96,1.44 0.76,1.64 0.76,3.44 0,3.96 -2.52,6.36 -2.4,2.36 -6.16,2.36 -2.8,0 -5.24,-1.88 -2.36,-1.88 -2.88,-5.44 0,-0.28 0.24,-0.28 l 4,0 q 0.12,0 0.2,0.16 0.56,1.36 1.4,2.2 0.88,0.84 2.52,0.84 0.8,0 1.48,-0.28 0.68,-0.28 1.32,-0.84 1.2,-1.08 1.2,-3.08 0,-2 -1.2,-3.08 -1.24,-1.12 -2.88,-1.12 l -1.68,0 z"
            }
          ]
          -- 4
        , [ { mainHead = "m -34.661318,-18.010823"
            , distantHead = "m 15.726596,38.876532"
            , rest = " q -0.24,0 -0.24,-0.2 l 0,-3.84 9.36,-19.96 q 0.12,-0.12 0.2,-0.12 l 4.44,0 q 0.12,0 0.2,0.12 0.08,0.08 0,0.2 l -9.24,19.6 6.64,0 0,-7.8 q 0,-0.2 0.24,-0.2 l 4,0 q 0.24,0 0.24,0.2 l 0,7.8 2,0 q 0.2,0 0.2,0.24 l 0,3.76 q 0,0.2 -0.2,0.2 l -2,0 0,3.92 q 0,0.2 -0.24,0.2 l -4,0 q -0.24,0 -0.24,-0.2 l 0,-3.92 -11.36,0 z"
            }
          ]
          -- 5
        , [ { mainHead = "m -33.181318,-26.650823"
            , distantHead = "m 17.206596,30.236532"
            , rest = " q -0.2,0 -0.2,-0.2 l 0,-15.04 q 0,-0.24 0.2,-0.24 l 14.8,0 q 0.24,0 0.24,0.24 l 0,3.96 q 0,0.24 -0.24,0.24 l -10.8,0 0,5.6 q 1.84,-1.32 4.2,-1.32 1.6,0 3,0.64 1.36,0.68 2.16,1.48 1.12,1.24 1.68,2.8 0.56,1.56 0.56,5 0,1.4 -0.12,2.48 -0.08,1.04 -0.32,1.76 l 0.04,-0.04 q -0.4,1.36 -1.36,2.6 -0.96,1.12 -2.48,2.08 -1.56,0.96 -3.72,0.96 -2.8,0 -5.32,-1.68 -2.64,-1.76 -3.04,-5.72 -0.08,-0.28 0.24,-0.28 l 3.96,0 q 0.24,0 0.24,0.2 0.68,3.08 3.76,3.08 2.8,0 3.16,-1.84 0.52,-1.96 0.52,-3.92 0,-2.16 -0.76,-3.76 -0.72,-1.36 -3.04,-1.36 -2.16,0 -3.4,2.16 -0.08,0.12 -0.2,0.12 l -3.76,0 z"
            }
          ]
          -- 6
        , [ { mainHead = "m -33.229037,-34.258584"
            , distantHead = "m 16.926596,22.756532"
            , rest = " q 0.08,-3.88 2.48,-6.12 1.16,-1 2.52,-1.56 1.4,-0.56 2.92,-0.56 2.76,0 5,1.68 2.08,1.64 2.76,4.84 0.08,0.28 -0.24,0.28 l -4,0 q -0.12,0 -0.2,-0.16 -0.88,-2.12 -3.36,-2.2 -3.52,0.08 -3.52,4.04 l 0,2.32 q 1.28,-0.36 2.52,-0.56 1.24,-0.24 2.4,-0.2 2.72,0 4.68,1.88 1.88,2.04 1.84,6.08 l 0,2.56 q -0.08,4 -2.36,6.08 -2.24,2.12 -5.52,2.16 -3.2,-0.04 -5.44,-2.12 -2.44,-2.16 -2.48,-6.12 l 0,-12.32 z m 4.36,12.08 q 0,4.04 3.56,4.04 3.44,0 3.6,-4.04 l 0,-2.28 q -0.16,-3.92 -3.6,-4.04 -3.56,0.12 -3.56,4.04 l 0,2.28 z"
            }
          ]
          -- 7
        , [ { mainHead = "m -33.101318,-33.690823"
            , distantHead = "m 19.028759,23.196532"
            , rest = " q -0.24,0 -0.24,-0.2 l 0,-8 q 0,-0.24 0.24,-0.24 l 15.8,0 q 0.24,0 0.24,0.24 l 0,4 -9.44,23.88 q -0.08,0.12 -0.2,0.12 l -4.48,0 q -0.28,0 -0.2,-0.28 l 9.32,-23.52 -6.84,0 0,3.8 q 0,0.2 -0.24,0.2 l -3.96,0 z"
            }
          ]
          -- 8
        , [ { mainHead = "m -28.920607,-34.6354"
            , distantHead = "m 21.118885,22.716532"
            , rest = " q 0,1.84 1.12,2.8 1.2,0.96 2.64,0.96 1.44,0 2.6,-0.96 1.16,-0.96 1.16,-2.8 0,-1.84 -1.16,-2.8 -1.16,-0.96 -2.6,-0.96 -1.44,0 -2.64,0.96 -1.12,0.96 -1.12,2.8 z m -4.92,11.96 q 0,-4 3.12,-6.28 -1.2,-1.12 -1.92,-2.44 -0.72,-1.36 -0.72,-3.16 0,-3.52 2.32,-5.88 2.4,-2.4 5.88,-2.4 3.44,0 5.84,2.4 2.36,2.48 2.36,5.88 0,1.8 -0.76,3.16 -0.72,1.32 -1.88,2.44 3.16,2.28 3.16,6.28 0,3.72 -2.6,6.2 -1.28,1.16 -2.84,1.76 -1.52,0.6 -3.28,0.6 -3.64,0 -6.12,-2.36 -2.56,-2.4 -2.56,-6.2 z m 4.4,-0.04 q 0,0.92 0.36,1.68 0.36,0.76 0.92,1.32 1.4,1.2 3,1.2 1.64,0 2.96,-1.2 1.32,-1.24 1.32,-3 0,-1.8 -1.32,-3 -1.2,-1.2 -2.96,-1.2 -1.68,0 -3,1.2 -1.28,1.2 -1.28,3 z"
            }
          ]
          -- 9
        , [ { mainHead = "m -33.112891,-34.5954"
            , distantHead = "m 16.926596,22.756532"
            , rest = " q 0.08,-3.88 2.48,-6.12 1.16,-1 2.52,-1.56 1.4,-0.56 2.92,-0.56 3.16,0 5.52,2.12 2.28,2.12 2.36,6.12 l 0,12.32 q -0.04,1.92 -0.68,3.48 -0.6,1.52 -1.68,2.6 -1.12,1 -2.52,1.56 -1.4,0.56 -3,0.6 -2.76,-0.04 -4.92,-1.72 l 0,0.04 q -2.16,-1.64 -2.84,-4.84 -0.08,-0.24 0.24,-0.24 l 3.96,0 q 0.16,0 0.24,0.16 0.84,2.16 3.32,2.16 3.44,0 3.6,-4.04 l 0,-2.32 q -1.28,0.4 -2.56,0.64 -1.28,0.2 -2.4,0.16 -2.68,0.04 -4.68,-1.96 l 0.04,0 q -1.92,-1.92 -1.92,-6.04 l 0,-2.56 z m 4.36,2.52 q 0,3.96 3.56,4.04 3.48,-0.08 3.6,-4.04 l 0,-2.28 q -0.2,-4 -3.6,-4.04 -3.56,0.08 -3.56,4.04 l 0,2.28 z"
            }
          ]
          -- 10
        , [ { mainHead = "m -33.466729,-14.2904"
            , distantHead = "m 16.562871,37.094758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-18.85 q -0.7475,0.5525 -1.4625,1.105 -0.715,0.52 -1.4625,1.04 -0.0975,0.065 -0.195,0.0325 -0.0975,-0.0325 -0.0975,-0.1625 l 0,-3.445 q 0,-0.0975 0.065,-0.1625 l 3.2825,-2.3725 3.315,0 q 0.195,0 0.195,0.195 l 0,22.62 q 0,0.195 -0.195,0.195 l -3.25,0 z"
            }
          , { mainHead = "m -17.155283,-30.6054"
            , distantHead = "m 32.874317,20.779758"
            , rest =
                " q 0,-1.56 -0.8125,-2.405 -0.8125,-0.8775 -2.1775,-0.8775 -1.365,0 -2.1775,0.8775 -0.8125,0.845 -0.8125,2.405 l 0,9.5875 q 0,1.56 0.8125,2.4375 0.8125,0.845 2.1775,0.845 1.365,0 2.1775,-0.845 0.8125,-0.8775 0.8125,-2.4375 l 0,-9.5875 z m -9.5875,-0.195 q 0,-3.25 2.015,-4.94 1.9825,-1.755 4.5825,-1.755 2.535,0 4.5825,1.755 2.015,1.755 2.015,4.94 l 0,9.9775 q 0,3.2175 -2.015,4.94 -2.015,1.7225 -4.5825,1.7225 -2.6325,0 -4.5825,-1.7225 -2.015,-1.7225 -2.015,-4.94 l 0,-9.9775 z"
            }
          ]
          -- 11
        , [ { mainHead = "m -31.906252,-13.70917"
            , distantHead = "m 17.357493,39.50387"
            , rest = " q -0.2175,0 -0.2175,-0.2175 l 0,-21.025 q -0.83375,0.61625 -1.63125,1.2325 -0.7975,0.58 -1.63125,1.16 -0.10875,0.0725 -0.2175,0.03625 -0.10875,-0.03625 -0.10875,-0.18125 l 0,-3.8425 q 0,-0.10875 0.0725,-0.18125 l 3.66125,-2.64625 3.6975,0 q 0.2175,0 0.2175,0.2175 l 0,25.23 q 0,0.2175 -0.2175,0.2175 l -3.625,0 z"
            }
          , { mainHead = "m -19.657717,-13.70917"
            , distantHead = "m 29.606028,39.50387"
            , rest = " q -0.2175,0 -0.2175,-0.2175 l 0,-21.025 q -0.83375,0.61625 -1.63125,1.2325 -0.7975,0.58 -1.63125,1.16 -0.10875,0.0725 -0.2175,0.03625 -0.10875,-0.03625 -0.10875,-0.18125 l 0,-3.8425 q 0,-0.10875 0.0725,-0.18125 l 3.66125,-2.64625 3.6975,0 q 0.2175,0 0.2175,0.2175 l 0,25.23 q 0,0.2175 -0.2175,0.2175 l -3.625,0 z"
            }
          ]
          -- 12
        , [ { mainHead = "m -33.331974,-14.2904"
            , distantHead = "m 16.562871,37.094758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-18.85 q -0.7475,0.5525 -1.4625,1.105 -0.715,0.52 -1.4625,1.04 -0.0975,0.065 -0.195,0.0325 -0.0975,-0.0325 -0.0975,-0.1625 l 0,-3.445 q 0,-0.0975 0.065,-0.1625 l 3.2825,-2.3725 3.315,0 q 0.195,0 0.195,0.195 l 0,22.62 q 0,0.195 -0.195,0.195 l -3.25,0 z"
            }
          , { mainHead = "m -26.608029,-14.3554"
            , distantHead = "m 23.286817,37.029758"
            , rest = " q -0.195,0 -0.195,-0.1625 l 0,-3.12 8.8725,-11.115 0,0.0325 q 0.4875,-0.65 0.6175,-1.1375 0.1625,-0.52 0.1625,-1.0075 0,-1.105 -0.78,-2.08 -0.78,-0.9425 -2.145,-0.9425 -1.2675,0 -2.145,0.845 -0.455,0.39 -0.6825,0.975 -0.2275,0.5525 -0.2925,1.2675 0,0.195 -0.195,0.195 l -3.2175,0 q -0.195,0 -0.195,-0.195 0,-2.8275 1.9175,-4.745 1.885,-1.95 4.5825,-1.95 2.86,0 4.81,1.95 1.95,1.95 1.95,4.7125 0,2.3075 -1.3975,4.095 l 0,-0.0325 q -1.8525,2.3075 -3.5425,4.42 -1.6575,2.1125 -3.51,4.42 l 8.255,0 q 0.195,0 0.195,0.195 l 0,3.2175 q 0,0.1625 -0.195,0.1625 l -12.87,0 z"
            }
          ]
          -- 13
        , [ { mainHead = "m -33.274601,-14.2904"
            , distantHead = "m 16.734993,37.094758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-18.85 q -0.7475,0.5525 -1.4625,1.105 -0.715,0.52 -1.4625,1.04 -0.0975,0.065 -0.195,0.0325 -0.0975,-0.0325 -0.0975,-0.1625 l 0,-3.445 q 0,-0.0975 0.065,-0.1625 l 3.2825,-2.3725 3.315,0 q 0.195,0 0.195,0.195 l 0,22.62 q 0,0.195 -0.195,0.195 l -3.25,0 z"
            }
          , { mainHead = "m -22.000656,-24.5604"
            , distantHead = "m 28.008938,26.824758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-2.8275 q 0,-0.195 0.195,-0.195 l 1.105,0 q 1.5275,0 2.4375,-0.845 0.8775,-0.8775 0.8775,-2.1775 0,-1.3 -0.91,-2.1775 -0.91,-0.91 -2.21,-0.91 -0.91,0 -1.82,0.65 -0.8775,0.6825 -1.105,1.9175 0,0.1625 -0.1625,0.1625 l -3.25,0 q -0.195,0 -0.195,-0.195 0.195,-2.47 2.08,-4.3225 1.885,-1.82 4.3225,-1.82 3.055,0 5.005,2.0475 0.8775,1.0075 1.365,2.1775 0.4875,1.17 0.4875,2.405 0,1.365 -0.52,2.6325 -0.26,0.5525 -0.715,1.0725 -0.4225,0.4875 -1.0075,0.9425 0.65,0.455 1.105,1.0075 0.455,0.5525 0.78,1.17 0.6175,1.3325 0.6175,2.795 0,3.2175 -2.0475,5.1675 -1.95,1.9175 -5.005,1.9175 -2.275,0 -4.2575,-1.5275 -1.9175,-1.5275 -2.34,-4.42 0,-0.2275 0.195,-0.2275 l 3.25,0 q 0.0975,0 0.1625,0.13 0.455,1.105 1.1375,1.7875 0.715,0.6825 2.0475,0.6825 0.65,0 1.2025,-0.2275 0.5525,-0.2275 1.0725,-0.6825 0.975,-0.8775 0.975,-2.5025 0,-1.625 -0.975,-2.5025 -1.0075,-0.91 -2.34,-0.91 l -1.365,0 z"
            }
          ]
          -- 14
        , [ { mainHead = "m -33.274601,-14.2904"
            , distantHead = "m 16.161262,37.094758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-18.85 q -0.7475,0.5525 -1.4625,1.105 -0.715,0.52 -1.4625,1.04 -0.0975,0.065 -0.195,0.0325 -0.0975,-0.0325 -0.0975,-0.1625 l 0,-3.445 q 0,-0.0975 0.065,-0.1625 l 3.2825,-2.3725 3.315,0 q 0.195,0 0.195,0.195 l 0,22.62 q 0,0.195 -0.195,0.195 l -3.25,0 z"
            }
          , { mainHead = "m -27.200656,-17.7029"
            , distantHead = "m 22.235207,33.682258"
            , rest = " q -0.195,0 -0.195,-0.1625 l 0,-3.12 7.605,-16.2175 q 0.0975,-0.0975 0.1625,-0.0975 l 3.6075,0 q 0.0975,0 0.1625,0.0975 0.065,0.065 0,0.1625 l -7.5075,15.925 5.395,0 0,-6.3375 q 0,-0.1625 0.195,-0.1625 l 3.25,0 q 0.195,0 0.195,0.1625 l 0,6.3375 1.625,0 q 0.1625,0 0.1625,0.195 l 0,3.055 q 0,0.1625 -0.1625,0.1625 l -1.625,0 0,3.185 q 0,0.1625 -0.195,0.1625 l -3.25,0 q -0.195,0 -0.195,-0.1625 l 0,-3.185 -9.23,0 z"
            }
          ]
          -- 15
        , [ { mainHead = "m -33.733586,-14.2904"
            , distantHead = "m 16.562873,37.094758"
            , rest = " q -0.195,0 -0.195,-0.195 l 0,-18.85 q -0.7475,0.5525 -1.4625,1.105 -0.715,0.52 -1.4625,1.04 -0.0975,0.065 -0.195,0.0325 -0.0975,-0.0325 -0.0975,-0.1625 l 0,-3.445 q 0,-0.0975 0.065,-0.1625 l 3.2825,-2.3725 3.315,0 q 0.195,0 0.195,0.195 l 0,22.62 q 0,0.195 -0.195,0.195 l -3.25,0 z"
            }
          , { mainHead = "m -26.457141,-24.7229"
            , distantHead = "m 23.839319,26.662258"
            , rest = " q -0.1625,0 -0.1625,-0.1625 l 0,-12.22 q 0,-0.195 0.1625,-0.195 l 12.025,0 q 0.195,0 0.195,0.195 l 0,3.2175 q 0,0.195 -0.195,0.195 l -8.775,0 0,4.55 q 1.495,-1.0725 3.4125,-1.0725 1.3,0 2.4375,0.52 1.105,0.5525 1.755,1.2025 0.91,1.0075 1.365,2.275 0.455,1.2675 0.455,4.0625 0,1.1375 -0.0975,2.015 -0.065,0.845 -0.26,1.43 l 0.0325,-0.0325 q -0.325,1.105 -1.105,2.1125 -0.78,0.91 -2.015,1.69 -1.2675,0.78 -3.0225,0.78 -2.275,0 -4.3225,-1.365 -2.145,-1.43 -2.47,-4.6475 -0.065,-0.2275 0.195,-0.2275 l 3.2175,0 q 0.195,0 0.195,0.1625 0.5525,2.5025 3.055,2.5025 2.275,0 2.5675,-1.495 0.4225,-1.5925 0.4225,-3.185 0,-1.755 -0.6175,-3.055 -0.585,-1.105 -2.47,-1.105 -1.755,0 -2.7625,1.755 -0.065,0.0975 -0.1625,0.0975 l -3.055,0 z"
            }
          ]
        ]


viewFixed : Location -> Int -> Svg msg
viewFixed location speed =
    let
        speedSvgPaths =
            Maybe.withDefault [] (Array.get (speed - 1) digitPaths)

        mainPaths paths =
            List.map (\path -> Svg.path [ d (path.mainHead ++ path.rest) ] []) paths

        distantPaths paths =
            List.map (\path -> Svg.path [ d (path.distantHead ++ path.rest) ] []) paths
    in
        case location of
            DistantSignalLocation ->
                g [ transform "translate(10,12)" ]
                    [ polygon
                        [ Svg.Attributes.style "fill: #ff0; stroke: black;stroke-width: 0.5"
                        , points "58.785,7.09 25.184,67.3425 -8.4165,7.09"
                        ]
                        []
                    , polygon [ points "51.688,11.2485 25.184,60.6465 -1.3195,11.2485" ] []
                    , g [ Svg.Attributes.style "fill: #ff0" ]
                        (distantPaths speedSvgPaths)
                    ]

            MainSignalLocation ->
                g [ transform "translate(60,70) rotate(180)" ]
                    [ polygon
                        [ Svg.Attributes.style "fill: #fff; stroke: black;stroke-width: 0.5"
                        , points "58.785,7.09 25.184,67.3425 -8.4165,7.09"
                        ]
                        []
                    , polygon [ points "51.688,11.2485 25.184,60.6465 -1.3195,11.2485" ]
                        []
                    , g [ Svg.Attributes.style "fill: #fff", transform "scale(-1,-1)" ]
                        (mainPaths speedSvgPaths)
                    ]
