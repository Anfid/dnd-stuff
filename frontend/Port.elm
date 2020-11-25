port module Port exposing (CalculateResponse, ParseError(..), Response(..), calculateDice, decodeResp, messageReceiver, sendMessage)

import Json.Decode as Decode exposing (Decoder, andThen, at, decodeString, fail, field, int, map, map2, oneOf, string)
import Json.Encode as Encode



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- IN


type Response
    = Calculate (Result ParseError CalculateResponse)


type alias CalculateResponse =
    { result : Int }


type ParseError
    = UnexpectedToken Int String
    | BadDie Int
    | IllegalExpression Int
    | UnmatchedParen Int
    | EmptyExpression Int


decodeResp : String -> Result Decode.Error Response
decodeResp =
    at [ "command" ] string |> andThen commandDecoder |> decodeString


commandDecoder : String -> Decoder Response
commandDecoder command =
    case command of
        "calculate_dice" ->
            map Calculate <|
                oneOf
                    [ at [ "error" ] string |> andThen parseErrorDecoder |> map Err
                    , map Ok calculateDecoder
                    ]

        _ ->
            fail "Unknown command"


calculateDecoder : Decoder CalculateResponse
calculateDecoder =
    map CalculateResponse (field "result" int)


parseErrorDecoder : String -> Decoder ParseError
parseErrorDecoder errkind =
    case errkind of
        "unexpected_token" ->
            map2 UnexpectedToken (field "index" int) (field "token" string)

        "bad_die" ->
            map BadDie (field "index" int)

        "illegal_expression" ->
            map IllegalExpression (field "index" int)

        "unmatched_paren" ->
            map UnmatchedParen (field "index" int)

        "empty_expression" ->
            map EmptyExpression (field "index" int)

        _ ->
            fail "Unexpected error kind"



-- OUT


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
