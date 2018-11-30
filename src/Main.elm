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
            [ { x = 36, y = 30, h = 6, w = 13, collidable = True, class = "boulder" }
            , { x = 36, y = 60, h = 4, w = 10, collidable = True, class = "boulder" }
            , { x = 16, y = 40, h = 5, w = 11, collidable = False, class = "mud" }
            , { x = 60, y = 80, h = 9, w = 11, collidable = False, class = "mud" }
            , { x = 100, y = 40, h = 10, w = 11, collidable = True, class = "boulder" }
            , { x = 70, y = 40, h = 8, w = 8, collidable = True, class = "fire" }
            ]
      , player = { v = 1, x = -10, y = -10, h = 16, w = 16, appearance = "character--going-down", r = 30 }
      }
    , Cmd.none
    )



-- JSON


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
