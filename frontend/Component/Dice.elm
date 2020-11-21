module Component.Dice exposing (Model, Msg, init, update, view)

import Element exposing (Element, centerX, centerY, column, el, height, none, px, spacing, text, width)
import Element.Input as Input
import Element.Region as Region
import Html.Events
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Port
import Session exposing (Session)
import Style exposing (buttonStyle, headingStyle, inputFieldStyle)


type alias Model =
    { expr : String
    }


init : Model
init =
    Model ""


type Msg
    = Expr String
    | Submit



-- UPDATE


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg _ model =
    case msg of
        Expr val ->
            ( { model | expr = val }, PageMsg.None, Cmd.none )

        Submit ->
            ( model, PageMsg.None, Port.calculateDice model.expr )



-- VIEW


view : Model -> Element Msg
view model =
    column
        [ spacing 16, centerX, centerY, height (px 500), width (px 300) ]
        [ el (headingStyle [ Region.heading 1 ]) (text "Dice")
        , Input.text (inputFieldStyle [ onEnter Submit ])
            { onChange = Expr
            , text = model.expr
            , placeholder = Just <| Input.placeholder [] <| text "2d20"
            , label = Input.labelAbove [] <| text "Expression:"
            }
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
