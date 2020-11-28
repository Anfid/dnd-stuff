port module Port exposing
    ( AnalyzeResponse
    , CalculateResponse
    , ParseError(..)
    , Response(..)
    , analyzeDice
    , calculateDice
    , decodeResp
    , messageReceiver
    , sendMessage
    )

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, andThen, array, at, decodeString, fail, field, float, int, map, map2, map4, oneOf, string)
import Json.Encode as Encode



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- IN


type Response
    = Calculate (Result ParseError CalculateResponse)
    | Analyze (Result ParseError AnalyzeResponse)


type alias CalculateResponse =
    { result : Int }


type alias AnalyzeResponse =
    { offset : Int
    , values : Array Float
    , total : Float
    , max : Float
    }


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

        "analyze_dice" ->
            map Analyze <|
                oneOf
                    [ at [ "error" ] string |> andThen parseErrorDecoder |> map Err
                    , map Ok analyzeDecoder
                    ]

        _ ->
            fail "Unknown command"


calculateDecoder : Decoder CalculateResponse
calculateDecoder =
    map CalculateResponse (field "result" int)


analyzeDecoder : Decoder AnalyzeResponse
analyzeDecoder =
    map4 AnalyzeResponse
        (at [ "result", "offset" ] int)
        (at [ "result", "values" ] (array float))
        (at [ "result", "total" ] float)
        (at [ "result", "max" ] float)


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
            Encode.object
                [ ( "command", Encode.string "calculate_dice" )
                , ( "expression", Encode.string expr )
                ]
                |> Encode.encode 0
    in
    sendMessage body


analyzeDice : String -> Cmd msg
analyzeDice expr =
    let
        body =
            Encode.object
                [ ( "command", Encode.string "analyze_dice" )
                , ( "expression", Encode.string expr )
                ]
                |> Encode.encode 0
    in
    sendMessage body
