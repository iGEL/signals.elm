module SelectChange exposing (onSelectChange)

import Json.Decode
import Html.Events


onSelectChange msg =
    Html.Events.on "change" (Json.Decode.map msg decoder)


decoder : Json.Decode.Decoder String
decoder =
    Html.Events.targetValue
        |> Json.Decode.andThen
            (\val ->
                Json.Decode.succeed val
            )
