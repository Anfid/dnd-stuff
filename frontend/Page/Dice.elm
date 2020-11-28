module Page.Dice exposing (Model, Msg, init, responseMsg, update, view)

import Array exposing (Array)
import Collage exposing (Collage)
import Collage.Render as Render
import Collage.Text as Text exposing (Text)
import Element exposing (Element, column, el, fill, none, px, row, text)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Port exposing (AnalyzeResponse, CalculateResponse, ParseError(..), Response)
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle, headingStyle, inputFieldStyle, redColor, textStyle)
import Util exposing (flip, onEnter)


expressionPlaceholder : String
expressionPlaceholder =
    "2d20"


type alias Model =
    { expr : String
    , throw : Maybe Int
    , data : Maybe AnalyzeData
    , error : Maybe ErrorInfo
    }


type alias AnalyzeData =
    { offset : Int
    , values : Array Float
    , total : Float
    , max : Float
    }


type alias ErrorInfo =
    { index : Int
    , description : String
    }


init : Model
init =
    Model "" Nothing Nothing Nothing


type Msg
    = Expr String
    | Throw
    | AnalyzeResponse AnalyzeResponse
    | ThrowResponse CalculateResponse
    | ErrorResponse ParseError


responseMsg : Response -> Msg
responseMsg resp =
    case resp of
        Port.Calculate calculateResponse ->
            case calculateResponse of
                Ok res ->
                    ThrowResponse res

                Err e ->
                    ErrorResponse e

        Port.Analyze analyzeResponse ->
            case analyzeResponse of
                Ok res ->
                    AnalyzeResponse res

                Err e ->
                    ErrorResponse e



-- UPDATE


update : Msg -> Session -> Model -> ( Model, Cmd Msg )
update msg _ model =
    case msg of
        Expr val ->
            ( { model | expr = val, throw = Nothing, error = Nothing, data = Nothing }
            , if String.isEmpty val then
                Cmd.none

              else
                Port.analyzeDice val
            )

        Throw ->
            let
                expr =
                    if String.isEmpty model.expr then
                        expressionPlaceholder

                    else
                        model.expr
            in
            ( { model | expr = expr, throw = Nothing }
            , Cmd.batch [ Port.calculateDice expr, Port.analyzeDice expr ]
            )

        AnalyzeResponse result ->
            ( { model | data = Just result }, Cmd.none )

        ThrowResponse result ->
            ( { model | throw = Just <| result.result }, Cmd.none )

        ErrorResponse error ->
            ( { model | error = Just <| errorToInfo error }, Cmd.none )


errorToInfo : ParseError -> ErrorInfo
errorToInfo error =
    case error of
        UnexpectedToken index token ->
            ErrorInfo index ("Unexpected token '" ++ token ++ "'")

        BadDie index ->
            ErrorInfo index "Bad die"

        IllegalExpression index ->
            ErrorInfo index "Illegal expression"

        UnmatchedParen index ->
            ErrorInfo index "Unmatched parenthesis"

        EmptyExpression index ->
            ErrorInfo index "Empty expression"



-- VIEW


view : Model -> Element Msg
view model =
    column
        [ Element.padding 20
        , Element.spacing 16
        , Element.centerX
        , Element.height fill
        , Element.width (fill |> Element.maximum 400)
        , Background.color bgColor
        ]
        [ el (headingStyle [ Region.heading 1 ]) (text "Dice")
        , Input.text (inputFieldStyle [ onEnter Throw, Element.centerX ])
            { onChange = Expr
            , text = model.expr
            , placeholder = Just <| Input.placeholder [] <| text expressionPlaceholder
            , label = Input.labelAbove [] <| el (textStyle []) <| text "Expression:"
            }
        , Input.button
            (buttonStyle [ Element.centerX, Element.height <| px 50, Element.width <| px 150 ])
            { onPress = Just Throw, label = el [ Element.centerX ] <| text "Throw" }
        , row (textStyle []) <|
            case ( model.error, model.throw ) of
                ( Just e, _ ) ->
                    [ el [ Element.transparent True ] (text <| String.slice 0 e.index model.expr) -- offset
                    , text ("^ " ++ e.description) -- error
                    ]

                ( Nothing, Just t ) ->
                    [ el (textStyle []) <| text <| "Throw: " ++ String.fromInt t ]

                ( Nothing, Nothing ) ->
                    [ el (textStyle []) <| text "" ]
        , case model.data of
            Just res ->
                dataToCollage res
                    |> Render.svg
                    |> Element.html
                    |> el [ Element.centerX, Element.width Element.shrink ]

            Nothing ->
                none
        ]


collageW : Float
collageW =
    200


collageH : Float
collageH =
    250


dataToCollage : AnalyzeData -> Collage msg
dataToCollage data =
    let
        len =
            Array.length data.values

        barWidth =
            collageW / toFloat len

        scaledChance v =
            collageH * v / data.max
    in
    Collage.group
        -- X scales
        [ intScale data.offset |> Collage.shift ( barWidth / 2, -15 )
        , intScale (data.offset + len // 2) |> Collage.shift ( collageW / 2, -15 )
        , intScale (data.offset + len - 1) |> Collage.shift ( collageW - barWidth / 2, -15 )

        -- Y scales
        , percentScale 0 |> Collage.shift ( -15, 0 )
        , percentScale (data.max / data.total) |> Collage.shift ( -15, collageH )

        -- Graph
        , Collage.path [ ( 0, collageH + 15 ), ( 0, 0 ), ( collageW + 15, 0 ) ]
            |> Collage.traced
                (Collage.solid 2 <| Collage.uniform <| Style.toColor Style.greenBrightColor)
        , data.values
            |> Array.indexedMap
                (\i v ->
                    [ ( toFloat i * barWidth, scaledChance v )
                    , ( toFloat (i + 1) * barWidth, scaledChance v )
                    ]
                )
            |> Array.push [ ( collageW, 0 ) ]
            |> Array.toList
            |> List.concat
            |> (::) ( 0, 0 )
            |> Collage.polygon
            |> Collage.filled
                (Collage.uniform <| Style.toColor redColor)
        ]


intScale : Int -> Collage msg
intScale =
    Collage.rendered << styleScale << Text.fromString << String.fromInt


percentScale : Float -> Collage msg
percentScale =
    Collage.rendered << styleScale << Text.fromString << flip (++) "%" << String.fromFloat << (*) 100


styleScale : Text -> Text
styleScale =
    Text.typeface Text.Monospace
        >> Text.color (Style.toColor Style.fgColor)
        >> Text.size Text.normal
