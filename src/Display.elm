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
                        "160" ->
                            concat ten sixty

                        "150" ->
                            concat ten fifty

                        "140" ->
                            concat ten fourty

                        "130" ->
                            concat ten thirty

                        "120" ->
                            concat ten twenty

                        "110" ->
                            concat ten ten

                        "100" ->
                            concat ten zero

                        "90" ->
                            ninety

                        "80" ->
                            eigthy

                        "70" ->
                            seventy

                        "60" ->
                            sixty

                        "50" ->
                            fifty

                        "40" ->
                            fourty

                        "30" ->
                            thirty

                        "20" ->
                            twenty

                        "10" ->
                            ten

                        _ ->
                            off

                Messages.Off ->
                    off
    in
        g []
            [ rect [ width "50", height "40", x "0", y "0", Svg.Attributes.style "fill:black; stroke: none" ] []
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
    tinyLamp color on (toString (5 * y + 5)) (toString (5 * x + 5))


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


off =
    [ [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    , [ False ]
    ]


ten =
    [ [ False, False, True ]
    , [ False, True, True ]
    , [ True, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    , [ False, False, True ]
    ]


twenty =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ False, False, False, True, False ]
    , [ False, False, True, False, False ]
    , [ False, True, False, False, False ]
    , [ True, True, True, True, True ]
    ]


thirty =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ False, True, True, True, False ]
    , [ False, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


fourty =
    [ [ False, False, False, True, False ]
    , [ False, False, True, True, False ]
    , [ False, True, False, True, False ]
    , [ True, False, False, True, False ]
    , [ True, True, True, True, True ]
    , [ False, False, False, True, False ]
    , [ False, False, False, True, False ]
    ]


fifty =
    [ [ True, True, True, True, True ]
    , [ True, False, False, False, False ]
    , [ True, True, True, True, False ]
    , [ False, False, False, False, True ]
    , [ False, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


sixty =
    [ [ False, False, True, True, False ]
    , [ False, True, False, False, False ]
    , [ True, False, False, False, False ]
    , [ True, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


seventy =
    [ [ True, True, True, True, True ]
    , [ False, False, False, False, True ]
    , [ False, False, False, True, False ]
    , [ False, False, True, False, False ]
    , [ False, True, False, False, False ]
    , [ False, True, False, False, False ]
    , [ False, True, False, False, False ]
    ]


eigthy =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]


ninety =
    List.map List.reverse (List.reverse sixty)


zero =
    [ [ False, True, True, True, False ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ True, False, False, False, True ]
    , [ False, True, True, True, False ]
    ]
