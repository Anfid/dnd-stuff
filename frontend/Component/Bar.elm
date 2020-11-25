module Component.Bar exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (Element, alignRight, padding, row, spacing, text)
import Element.Input exposing (button)
import PageMsg exposing (PageMsg)
import Session exposing (Session)
import Style exposing (buttonStyle)


init : Model
init =
    ()


type Msg
    = ShowThrowDice
    | ShowAnalyzeDice


type alias Model =
    ()


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        ShowThrowDice ->
            ( model, PageMsg.ShowThrowDice, Navigation.pushUrl session.key "/dice/throw" )

        ShowAnalyzeDice ->
            ( model, PageMsg.ShowAnalyzeDice, Navigation.pushUrl session.key "/dice/analyze" )


view : Model -> Element Msg
view _ =
    row [ alignRight, spacing 5, padding 5 ]
        [ button (buttonStyle [])
            { onPress = Just ShowThrowDice, label = text "Throw dice" }
        , button (buttonStyle [])
            { onPress = Just ShowAnalyzeDice, label = text "Analyze dice" }
        ]
