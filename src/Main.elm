module Main exposing (main)

import Browser
import Browser.Events as E
import Json.Decode as Decode
import Types exposing (..)
import Update exposing (..)
import View exposing (..)


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
    ( { items =
            [ { x = 360, y = 300, h = 40, w = 130, collidable = True }
            , { x = 360, y = 600, h = 40, w = 130, collidable = True }
            , { x = 160, y = 400, h = 40, w = 130, collidable = False }
            ]
      , player = { v = 8, x = 0, y = 0, h = 60, w = 60, appearance = "character--going-up", r = 300 }
      }
    , Cmd.none
    )



-- JSON


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
