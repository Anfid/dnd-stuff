module Component.AnalyzeDice exposing (Model, Msg, init, responseMsg, update, view)

import Array exposing (Array)
import Collage exposing (Collage)
import Collage.Render as Render
import Collage.Text as Text exposing (Text)
import Element exposing (Element, centerX, centerY, column, el, height, none, px, row, spacing, text, width)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import PageMsg exposing (PageMsg)
import Port exposing (AnalyzeResponse, ParseError(..))
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle, headingStyle, inputFieldStyle, redColor, textStyle)
import Util exposing (onEnter)


expressionPlaceholder : String
expressionPlaceholder =
    "2d20"


type alias Model =
    { expr : String
    , result : Maybe AnalyzeData
    , error : Maybe ErrorInfo
    }


type alias AnalyzeData =
    { offset : Int
    , values : Array Int
    , total : Int
    }


type alias ErrorInfo =
    { index : Int
    , description : String
    }


init : Model
init =
    Model "" Nothing Nothing


type Msg
    = Expr String
    | Submit
    | Response (Result ParseError AnalyzeResponse)


responseMsg : Result ParseError AnalyzeResponse -> Msg
responseMsg res =
    Response res



-- UPDATE


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg s model =
    case msg of
        Expr val ->
            update Submit s { model | expr = val, error = Nothing }

        --( { model | expr = val, error = Nothing }, PageMsg.None, Cmd.none )
        Submit ->
            let
                expr =
                    if String.isEmpty model.expr then
                        expressionPlaceholder

                    else
                        model.expr
            in
            ( { model | error = Nothing, result = Nothing }, PageMsg.None, Port.analyzeDice expr )

        Response result ->
            let
                new_model =
                    case result of
                        Ok res ->
                            { model
                                | result =
                                    Just
                                        { offset = res.offset
                                        , values = res.values
                                        , total = Array.foldl max 0 res.values
                                        }
                            }

                        Err error ->
                            { model | error = Just <| errorToInfo error }
            in
            ( new_model, PageMsg.None, Cmd.none )


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
        [ spacing 16, centerX, centerY, height (px 500), width (px 300), Background.color bgColor ]
        [ el (headingStyle [ Region.heading 1 ]) (text "Dice")
        , Input.text (inputFieldStyle [ onEnter Submit ])
            { onChange = Expr
            , text = model.expr
            , placeholder = Just <| Input.placeholder [] <| text expressionPlaceholder
            , label = Input.labelAbove [] <| el (textStyle []) <| text "Expression:"
            }
        , case model.error of
            Just e ->
                row (textStyle [])
                    [ el [ Element.transparent True ] (text <| String.slice 0 e.index model.expr) -- offset
                    , text ("^ " ++ e.description) -- error
                    ]

            Nothing ->
                none
        , Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Just Submit, label = el [ centerX ] <| text "Show" }
        , case model.result of
            Just res ->
                el (textStyle [ Background.color bgColor ]) <|
                    Element.html <|
                        Render.svg <|
                            dataToCollage res

            Nothing ->
                none
        ]


collageW : Float
collageW =
    300


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
            collageH * toFloat v / toFloat data.total
    in
    Collage.group
        [ scale data.offset |> Collage.shift ( barWidth / 2, -15 )
        , scale (data.offset + len // 2) |> Collage.shift ( collageW / 2, -15 )
        , scale (data.offset + len - 1) |> Collage.shift ( collageW - barWidth / 2, -15 )
        , Collage.path [ ( 0, collageH + 15 ), ( 0, 0 ), ( collageW + 15, 0 ) ]
            |> Collage.traced
                (Collage.solid 2 <| Collage.uniform <| Style.toColor Style.greenBrightColor)
        , Collage.filled
            (Collage.uniform <| Style.toColor redColor)
          <|
            Collage.polygon <|
                (::) ( 0, 0 ) <|
                    List.concat <|
                        Array.toList <|
                            Array.push [ ( collageW, 0 ) ] <|
                                Array.indexedMap
                                    (\i v ->
                                        [ ( toFloat i * barWidth, scaledChance v )
                                        , ( toFloat (i + 1) * barWidth, scaledChance v )
                                        ]
                                    )
                                    data.values
        ]


scale : Int -> Collage msg
scale =
    Collage.rendered << styleScale << Text.fromString << String.fromInt


styleScale : Text -> Text
styleScale =
    Text.typeface Text.Monospace >> Text.color (Style.toColor Style.fgColor) >> Text.size Text.normal
