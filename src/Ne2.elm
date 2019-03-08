module Ne2 exposing (ne2)

import Svg exposing (..)
import Svg.Attributes exposing (..)


ne2 : Bool -> Svg msg
ne2 semaphoreShortendBreakPath =
    g []
        (List.append
            (if semaphoreShortendBreakPath then
                [ Svg.path [ d "m1,-23 17,20 17,-20 z", class "ral9002", Svg.Attributes.style "stroke:#0e0e10;stroke-width:2" ] [] ]

             else
                []
            )
            [ rect
                [ class "ral9002"
                , Svg.Attributes.style "stroke:#0e0e10;stroke-width:2;stroke-miterlimit:4"
                , width "34"
                , height "54"
                , x "1"
                , y "0"
                ]
                []
            , Svg.path [ d "m 0,55 18,-27.5 18,27.5 -7,0 -11,-17.5 -11,17.5 z", class "ral9005" ] []
            , Svg.path [ d "m 36,0 -18,27.5 -18,-27.5 7,0 11,17.5 11,-17.5 z", class "ral9005" ] []
            ]
        )
