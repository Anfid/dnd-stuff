module Util exposing (flip, onEnter)

import Element
import Html.Events
import Json.Decode as Decode


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
