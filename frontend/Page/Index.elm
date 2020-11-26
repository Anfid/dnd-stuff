module Page.Index exposing (Model, Msg, init, responseMsg, update, view)

import Component.Bar as BarComponent
import Component.Dice as DiceComponent
import Element exposing (fill, spacing, width)
import Element.Background as Background
import Html exposing (Html)
import PageMsg exposing (PageMsg)
import Port exposing (Response)
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
    | ExternalResponse Response


responseMsg : Response -> Msg
responseMsg resp =
    ExternalResponse resp


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        BarMsg m ->
            handleUpdate updateBarModel BarMsg model <| BarComponent.update m session model.bar

        DiceMsg m ->
            handleUpdate updateDiceModel DiceMsg model <| DiceComponent.update m session model.dice

        ExternalResponse resp ->
            case resp of
                Port.Calculate calculateResponse ->
                    let
                        componentMsg =
                            case calculateResponse of
                                Ok res ->
                                    DiceComponent.calculateResponseMsg res

                                Err e ->
                                    DiceComponent.errorResponseMsg e
                    in
                    handleUpdate updateDiceModel DiceMsg model <|
                        DiceComponent.update
                            componentMsg
                            session
                            model.dice

                Port.Analyze analyzeResponse ->
                    let
                        componentMsg =
                            case analyzeResponse of
                                Ok res ->
                                    DiceComponent.analyzeResponseMsg res

                                Err e ->
                                    DiceComponent.errorResponseMsg e
                    in
                    handleUpdate updateDiceModel DiceMsg model <|
                        DiceComponent.update
                            componentMsg
                            session
                            model.dice


handleUpdate :
    (subModel -> Model -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, PageMsg, Cmd subMsg )
    -> ( Model, PageMsg, Cmd Msg )
handleUpdate toModel toMsg model ( subModel, pageMsg, subCmd ) =
    ( toModel subModel model, pageMsg, Cmd.map toMsg subCmd )


updateBarModel : BarComponent.Model -> Model -> Model
updateBarModel barModel model =
    { model | bar = barModel }


updateDiceModel : DiceComponent.Model -> Model -> Model
updateDiceModel diceModel model =
    { model | dice = diceModel }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color bgColor, Element.scrollbarY, Element.height Element.fill ] <|
        Element.column [ width fill, spacing 40, Background.color bgColor ]
            [ Element.map BarMsg <| BarComponent.view model.bar
            , Element.map DiceMsg <| DiceComponent.view model.dice
            ]
