module Page.Index exposing (Model, Msg, init, responseMsg, update, view)

import Component.Bar as BarComponent
import Component.ThrowDice as ThrowDiceComponent
import Element exposing (fill, spacing, width)
import Element.Background as Background
import Html exposing (Html)
import PageMsg exposing (PageMsg)
import Port exposing (Response(..))
import Session exposing (Session)
import Style exposing (bgColor)



-- MODEL


type alias Model =
    { bar : BarComponent.Model
    , throwDice : ThrowDiceComponent.Model
    }


init : Model
init =
    { bar = BarComponent.init
    , throwDice = ThrowDiceComponent.init
    }



-- UPDATE


type Msg
    = BarMsg BarComponent.Msg
    | ThrowDiceMsg ThrowDiceComponent.Msg
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

        ExternalResponse resp ->
            case resp of
                Calculate calculateResponse ->
                    handleUpdate updateThrowDiceModel ThrowDiceMsg model <|
                        ThrowDiceComponent.update
                            (ThrowDiceComponent.responseMsg calculateResponse)
                            session
                            model.throwDice


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
                    { model | throwDice = ThrowDiceComponent.init }

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



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color bgColor ] <|
        Element.column [ width fill, spacing 40 ]
            [ Element.map BarMsg <| BarComponent.view model.bar
            , Element.map ThrowDiceMsg <| ThrowDiceComponent.view model.throwDice
            ]
