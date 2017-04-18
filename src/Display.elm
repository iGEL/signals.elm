module Display exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Lamp exposing (..)
import Messages exposing (DisplayMsg)


view : String -> Messages.DisplayMsg -> Svg msg
view color msg =
    let
        matrix =
            case msg of
                Messages.On speed ->
                    case speed of
                        "15" ->
                            concat ten fifty

                        "14" ->
                            concat ten fourty

                        "13" ->
                            concat ten thirty

                        "12" ->
                            concat ten twenty

                        "11" ->
                            concat ten ten

                        "10" ->
                            concat ten zero

                        "9" ->
                            ninety

                        "8" ->
                            eigthy

                        "7" ->
                            seventy

                        "6" ->
                            sixty

                        "5" ->
                            fifty

                        "4" ->
                            fourty

                        "3" ->
                            thirty

                        "2" ->
                            twenty

                        "1" ->
                            ten

                        _ ->
                            off

                Messages.Off ->
                    off
    in
        g []
            [ rect [ width "54", height "44", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
            , g []
                (List.concat
                    (List.indexedMap (paintRow color) (center matrix))
                )
            ]


paintRow : String -> Int -> List Bool -> List (Svg msg)
paintRow color x row =
    List.indexedMap (paintLamp color x) row


paintLamp : String -> Int -> Int -> Bool -> Svg msg
paintLamp color x y on =
    tinyLamp color on (toString (5 * y + 7)) (toString (5 * x + 7))


center : List (List Bool) -> List (List Bool)
center matrix =
    let
        firstRow =
            Maybe.withDefault [] (List.head matrix)

        colsToPad =
            floor (toFloat (9 - List.length firstRow) / 2)

        pad =
            List.repeat colsToPad False
    in
        List.map
            (\row -> List.append pad (List.append row pad))
            matrix


concat : List (List Bool) -> List (List Bool) -> List (List Bool)
concat first second =
    let
        mergeLine =
            \a b -> List.append a (False :: b)
    in
        List.map2 mergeLine first second


off : List (List Bool)
off =
    [ [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    ]


ten : List (List Bool)
ten =
    [ [ False, False, True ]
    , [ False, True, True ]
    , [ True, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    ]


twenty : List (List Bool)
twenty =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ False, False, False, True, False ]
    , [ False, False, True, False, False ]
    , [ False, True, False, False, False ]
    , [ True, True, True, True, True ]
    ]


thirty : List (List Bool)
thirty =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ False, True, True, True, False ]
    , [ False, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


fourty : List (List Bool)
fourty =
    [ [ False, False, False, True, False ]
    , [ False, False, True, True, False ]
    , [ False, True, False, True, False ]
    , [ True, False, False, True, False ]
    , [ True, True, True, True, True ]
    , [ False, False, False, True, False ]
    , [ False, False, False, True, False ]
    ]


fifty : List (List Bool)
fifty =
    [ [ True, True, True, True, True ]
    , [ True, False, False, False, False ]
    , [ True, True, True, True, False ]
    , [ False, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


sixty : List (List Bool)
sixty =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, False ]
    , [ True, False, False, False, False ]
    , [ True, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


seventy : List (List Bool)
seventy =
    [ [ True, True, True, True, True ]
    , [ False, False, False, False, True ]
    , [ False, False, False, True, False ]
    , [ False, False, True, False, False ]
    , [ False, True, False, False, False ]
    , [ False, True, False, False, False ]
    , [ False, True, False, False, False ]
    ]


eigthy : List (List Bool)
eigthy =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


ninety : List (List Bool)
ninety =
    List.map List.reverse (List.reverse sixty)


zero : List (List Bool)
zero =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]
