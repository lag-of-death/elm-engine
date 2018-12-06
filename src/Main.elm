module Main exposing (main)

import Browser
import Browser.Events as E
import Json.Decode as Decode
import Time
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
        , if not (List.isEmpty model.player.closeEnemies) then
            Time.every 100 Tick

          else
            Sub.none
        ]



-- INIT


init : {} -> ( Model, Cmd Msg )
init flags =
    let
        enemies =
            [ { hp = 10, entity = { id = 0, x = 0, y = 0, h = 6, w = 8, class = "enemy_1", collidable = True } }
            , { hp = 10, entity = { id = 1, collidable = True, x = -10, y = 40, h = 8, w = 8, class = "enemy_1" } }
            , { hp = 10, entity = { id = 2, collidable = True, x = -20, y = 40, h = 6, w = 6, class = "enemy_2" } }
            , { hp = 10, entity = { id = 3, collidable = True, x = -30, y = 40, h = 8, w = 8, class = "enemy_1" } }
            , { hp = 10, entity = { id = 4, collidable = True, x = -40, y = 40, h = 4, w = 4, class = "enemy_2" } }
            , { hp = 10, entity = { id = 5, collidable = True, x = -50, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { id = 6, collidable = True, x = -60, y = 40, h = 6, w = 4, class = "enemy_1" } }
            ]

        closeEnemies =
            []

        player =
            { closeEnemies = closeEnemies, v = 1, entity = { x = -10, y = -10, h = 16, w = 16, class = "character--going-down", collidable = True, id = 123 }, r = 30 }
    in
    ( { items =
            [ { description = "", entity = { id = 0, x = 36, y = 30, h = 6, w = 13, class = "boulder", collidable = True } }
            , { description = "", entity = { id = 1, x = 36, y = 60, h = 4, w = 10, class = "boulder", collidable = True } }
            , { description = "", entity = { id = 2, collidable = False, x = 16, y = 40, h = 5, w = 11, class = "mud" } }
            , { description = "", entity = { id = 3, collidable = False, x = 60, y = 80, h = 9, w = 11, class = "mud" } }
            , { description = "", entity = { id = 4, collidable = True, x = 100, y = 40, h = 10, w = 11, class = "boulder" } }
            , { description = "", entity = { id = 5, collidable = True, x = 70, y = 40, h = 8, w = 8, class = "fire" } }
            ]
      , player = player
      , enemies = enemies
      }
    , Cmd.none
    )



-- JSON


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
