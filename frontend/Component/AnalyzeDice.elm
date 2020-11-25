module Component.AnalyzeDice exposing (Model, Msg, init, responseMsg, update, view)

import Array exposing (Array)
import Element exposing (Element, centerX, centerY, column, el, height, none, px, row, spacing, text, width)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import PageMsg exposing (PageMsg)
import Port exposing (AnalyzeResponse, ParseError(..))
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle, headingStyle, inputFieldStyle, textStyle)
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
update msg _ model =
    case msg of
        Expr val ->
            ( { model | expr = val, error = Nothing }, PageMsg.None, Cmd.none )

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
                            case error of
                                UnexpectedToken index token ->
                                    { model | error = Just <| ErrorInfo index ("Unexpected token '" ++ token ++ "'") }

                                BadDie index ->
                                    { model | error = Just <| ErrorInfo index "Bad die" }

                                IllegalExpression index ->
                                    { model | error = Just <| ErrorInfo index "Illegal expression" }

                                UnmatchedParen index ->
                                    { model | error = Just <| ErrorInfo index "Unmatched parenthesis" }

                                EmptyExpression index ->
                                    { model | error = Just <| ErrorInfo index "Empty expression" }
            in
            ( new_model, PageMsg.None, Cmd.none )



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
            , label = Input.labelAbove [] <| text "Expression:"
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
                column (textStyle [ Background.color bgColor ])
                    (Array.toList <| Array.map (\v -> text <| nHashes <| percent v res.total) res.values)

            Nothing ->
                none
        ]


percent : Int -> Int -> Int
percent val total =
    round <| 50 * toFloat val / toFloat total


nHashes : Int -> String
nHashes n =
    case n of
        0 ->
            ""

        _ ->
            "#" ++ nHashes (n - 1)
