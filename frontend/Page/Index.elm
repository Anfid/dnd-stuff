module Page.Index exposing (Model, Msg, init, responseMsg, update, view)

import Component.AnalyzeDice as AnalyzeDiceComponent
import Component.Bar as BarComponent
import Component.ThrowDice as ThrowDiceComponent
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
    , throwDice : ThrowDiceComponent.Model
    , analyzeDice : AnalyzeDiceComponent.Model
    , shown : ActiveComponent
    }


type ActiveComponent
    = Throw
    | Analyze


init : Model
init =
    { bar = BarComponent.init
    , throwDice = ThrowDiceComponent.init
    , analyzeDice = AnalyzeDiceComponent.init
    , shown = Analyze
    }



-- UPDATE


type Msg
    = BarMsg BarComponent.Msg
    | ThrowDiceMsg ThrowDiceComponent.Msg
    | AnalyzeDiceMsg AnalyzeDiceComponent.Msg
    | ExternalResponse Response


responseMsg : Response -> Msg
responseMsg resp =
    ExternalResponse resp


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        BarMsg m ->
            handleUpdate updateBarModel BarMsg model <| BarComponent.update m session model.bar

        ThrowDiceMsg m ->
            handleUpdate updateThrowDiceModel ThrowDiceMsg model <| ThrowDiceComponent.update m session model.throwDice

        AnalyzeDiceMsg m ->
            handleUpdate updateAnalyzeDiceModel AnalyzeDiceMsg model <| AnalyzeDiceComponent.update m session model.analyzeDice

        ExternalResponse resp ->
            case resp of
                Port.Calculate calculateResponse ->
                    handleUpdate updateThrowDiceModel ThrowDiceMsg model <|
                        ThrowDiceComponent.update
                            (ThrowDiceComponent.responseMsg calculateResponse)
                            session
                            model.throwDice

                Port.Analyze analyzeResponse ->
                    handleUpdate updateAnalyzeDiceModel AnalyzeDiceMsg model <|
                        AnalyzeDiceComponent.update
                            (AnalyzeDiceComponent.responseMsg analyzeResponse)
                            session
                            model.analyzeDice


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
                PageMsg.ShowThrowDice ->
                    { model | shown = Throw }

                PageMsg.ShowAnalyzeDice ->
                    { model | shown = Analyze }

                _ ->
                    model
    in
    ( toModel subModel newModel, pageMsg, Cmd.map toMsg subCmd )


updateBarModel : BarComponent.Model -> Model -> Model
updateBarModel barModel model =
    { model | bar = barModel }


updateThrowDiceModel : ThrowDiceComponent.Model -> Model -> Model
updateThrowDiceModel throwDiceModel model =
    { model | throwDice = throwDiceModel }


updateAnalyzeDiceModel : AnalyzeDiceComponent.Model -> Model -> Model
updateAnalyzeDiceModel analyzeDiceModel model =
    { model | analyzeDice = analyzeDiceModel }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color bgColor ] <|
        Element.column [ width fill, spacing 40, Background.color bgColor ]
            [ Element.map BarMsg <| BarComponent.view model.bar
            , case model.shown of
                Analyze ->
                    Element.map AnalyzeDiceMsg <| AnalyzeDiceComponent.view model.analyzeDice

                Throw ->
                    Element.map ThrowDiceMsg <| ThrowDiceComponent.view model.throwDice
            ]
