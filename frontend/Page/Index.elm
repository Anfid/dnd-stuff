module Page.Index exposing (Model, Msg, init, update, view)

import Component.Bar as BarComponent
import Component.Dice as DiceComponent
import Element exposing (fill, none, spacing, width)
import Element.Background as Background
import Html exposing (Html)
import PageMsg exposing (PageMsg)
import Session exposing (Session)
import Style exposing (bgColor)



-- MODEL


type alias Model =
    { bar : BarComponent.Model
    , dice : DiceComponent.Model
    }


init : Model
init =
    { bar = BarComponent.init
    , dice = DiceComponent.init
    }



-- UPDATE


type Msg
    = BarMsg BarComponent.Msg
    | DiceMsg DiceComponent.Msg


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        BarMsg m ->
            handleUpdate updateBarModel BarMsg model <| BarComponent.update m session model.bar

        DiceMsg m ->
            handleUpdate updateDiceModel DiceMsg model <| DiceComponent.update m session model.dice


handleUpdate :
    (subModel -> Model -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, PageMsg, Cmd subMsg )
    -> ( Model, PageMsg, Cmd Msg )
handleUpdate toModel toMsg model ( subModel, pageMsg, subCmd ) =
    let
        newModel =
            case pageMsg of
                PageMsg.ShowDice ->
                    { model | dice = DiceComponent.init }

                _ ->
                    model
    in
    ( toModel subModel newModel, pageMsg, Cmd.map toMsg subCmd )


updateBarModel : BarComponent.Model -> Model -> Model
updateBarModel barModel model =
    { model | bar = barModel }


updateDiceModel : DiceComponent.Model -> Model -> Model
updateDiceModel diceModel model =
    { model | dice = diceModel }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color bgColor ] <|
        Element.column [ width fill, spacing 40 ]
            [ Element.map BarMsg <| BarComponent.view model.bar
            , Element.map DiceMsg <| DiceComponent.view model.dice
            ]
