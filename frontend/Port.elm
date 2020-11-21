port module Port exposing (calculateDice, messageReceiver, sendMessage)

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, list, map, map2, map3, oneOf, string, succeed)
import Json.Encode as Encode



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- INTERFACE


calculateDice : String -> Cmd msg
calculateDice expr =
    let
        body =
            Encode.encode 0 <|
                Encode.object
                    [ ( "command", Encode.string "calculate_dice" )
                    , ( "expression", Encode.string expr )
                    ]
    in
    sendMessage body
