module Main exposing (main)

import Browser
import Browser.Events as E
import Html
import Html.Attributes as A
import Json.Decode as Decode


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ E.onKeyDown (Decode.map KeyDown keyDecoder)
        ]



-- INIT


init : {} -> ( Model, Cmd Msg )
init flags =
    ( "character--resting", Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown a ->
            case a of
                "ArrowRight" ->
                    ( "character--wounded", Cmd.none )

                "ArrowUp" ->
                    ( "character--healed", Cmd.none )

                "ArrowDown" ->
                    ( "character--fighting", Cmd.none )

                "ArrowLeft" ->
                    ( "character--resting", Cmd.none )

                _ ->
                    ( "character--resting", Cmd.none )



-- TYPES


type alias Model =
    String


type Msg
    = KeyDown String



-- VIEW


view : Model -> { body : List (Html.Html Msg), title : String }
view model =
    { body = [ Html.div [ A.class model ] [] ]
    , title = ""
    }



-- JSON


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
