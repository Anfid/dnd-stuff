module Main exposing (main)

import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Element
import Element.Background as Background
import Html
import Page.Dice
import Port exposing (decodeResp, messageReceiver)
import Session exposing (Session)
import Style
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)



-- MAIN


main : Program Dimensions Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }



-- MODEL


init : Dimensions -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init keys url key =
    ( gotoUrl url <| Model (Session key) keys url <| Dice <| Page.Dice.init, Cmd.none )


type alias Model =
    { session : Session
    , dimensions : Dimensions
    , url : Url
    , page : Page
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type Page
    = Dice Page.Dice.Model



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | Resize Int Int
    | DiceMsg Page.Dice.Msg
    | Recv String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            ( gotoUrl url model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( Resize w h, _ ) ->
            ( { model | dimensions = Dimensions w h }, Cmd.none )

        ( DiceMsg subMsg, Dice subModel ) ->
            Page.Dice.update subMsg model.session subModel
                |> handleUpdate Dice DiceMsg model

        ( Recv str, Dice subModel ) ->
            case decodeResp str of
                Ok response ->
                    Page.Dice.update (Page.Dice.responseMsg response) model.session subModel
                        |> handleUpdate Dice DiceMsg model

                Err _ ->
                    ( model, Cmd.none )


handleUpdate : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
handleUpdate toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Dice subModel ->
            { title = "Dice"
            , body =
                [ subModel
                    |> Page.Dice.view
                    |> Element.layout [ Background.color Style.bgColor, Element.scrollbarY, Element.height Element.fill ]
                    |> Html.map DiceMsg
                ]
            }



-- ROUTER


type Route
    = IndexRoute


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map IndexRoute Parser.top
        ]


gotoUrl : Url -> Model -> Model
gotoUrl url model =
    case Parser.parse parser url of
        Just IndexRoute ->
            { model | page = Dice <| Page.Dice.init, url = url }

        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , messageReceiver Recv
        ]
