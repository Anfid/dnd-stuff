module Component.ThrowDice exposing (Model, Msg, init, responseMsg, update, view)

import Element exposing (Element, alignLeft, centerX, centerY, column, el, height, none, px, row, spacing, text, width)
import Element.Input as Input
import Element.Region as Region
import Html.Events
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Port exposing (CalculateResponse, ParseError(..))
import Session exposing (Session)
import Style exposing (buttonStyle, headingStyle, inputFieldStyle, textStyle)


expressionPlaceholder =
    "2d20"


type alias Model =
    { expr : String
    , result : String
    , error : Maybe ErrorInfo
    }


type alias ErrorInfo =
    { index : Int
    , description : String
    }


init : Model
init =
    Model "" "" Nothing


type Msg
    = Expr String
    | Submit
    | Response (Result ParseError CalculateResponse)


responseMsg : Result ParseError CalculateResponse -> Msg
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
            ( { model | error = Nothing, result = "" }, PageMsg.None, Port.calculateDice expr )

        Response result ->
            let
                new_model =
                    case result of
                        Ok res ->
                            { model | result = String.fromInt res.result }

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
        [ spacing 16, centerX, centerY, height (px 500), width (px 300) ]
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
                el (textStyle [ centerX ]) (text model.result)
        , Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Just Submit, label = el [ centerX ] <| text "Throw" }
        ]


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
