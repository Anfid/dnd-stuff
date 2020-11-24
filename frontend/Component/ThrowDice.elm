module Component.ThrowDice exposing (Model, Msg, init, responseMsg, update, view)

import Element exposing (Element, centerX, centerY, column, el, height, none, px, spacing, text, width)
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
    }


init : Model
init =
    Model "" ""


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
            ( { model | expr = val }, PageMsg.None, Cmd.none )

        Submit ->
            let
                expr =
                    if String.isEmpty model.expr then
                        expressionPlaceholder

                    else
                        model.expr
            in
            ( model, PageMsg.None, Port.calculateDice expr )

        Response result ->
            let
                str =
                    case result of
                        Ok res ->
                            String.fromInt res.result

                        Err error ->
                            case error of
                                UnexpectedToken { index, token } ->
                                    "Error: Unexpected token '" ++ token ++ "' at character " ++ String.fromInt (index + 1)

                                BadDie index ->
                                    "Error: Bad die at character " ++ String.fromInt (index + 1)

                                IllegalExpression index ->
                                    "Error: Illegal expression at character " ++ String.fromInt (index + 1)

                                UnmatchedParen index ->
                                    "Error: Unmatched parenthesis at character " ++ String.fromInt (index + 1)

                                EmptyExpression index ->
                                    "Error: Empty expression at character " ++ String.fromInt (index + 1)
            in
            ( { model | result = str }, PageMsg.None, Cmd.none )



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
        , el (textStyle [ centerX ]) (text model.result)
        , Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Just Submit, label = el [ centerX ] <| text "Submit" }
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
