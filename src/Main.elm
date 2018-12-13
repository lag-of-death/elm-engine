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
        , Time.every 1000 Second
        , if not (model.player.direction == "none") then
            Time.every 50 (\_ -> Walk model.player.direction)

          else
            Sub.none
        ]



-- INIT


init : {} -> ( Model, Cmd Msg )
init flags =
    let
        enemies =
            [ { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 6 }, id = 0, x = 0, y = 0, h = 6, w = 8, class = "enemy_1", collidable = True } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -10, y = 40, h = 8, w = 8, class = "enemy_1" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -20, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -30, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -40, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -50, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -60, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -70, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -80, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { hp = 10, entity = { bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 1, collidable = True, x = -90, y = 40, h = 8, w = 8, class = "enemy_2" } }
            ]

        closeEnemies =
            []

        player =
            { direction = "none", chase = { x = 0, y = 0 }, closeEnemies = closeEnemies, v = 1, entity = { bounds = { w = 8, h = 10, x = 4, y = 5 }, x = 10, y = 30, h = 16, w = 16, class = "character--going-down", collidable = True, id = 123 }, r = 50 }
    in
    ( { items =
            [ { description = "", entity = { bounds = { x = 0, y = 0, w = 10, h = 4 }, id = 1, x = 36, y = 60, h = 4, w = 10, class = "boulder", collidable = True } }
            , { description = "", entity = { id = 2, collidable = False, x = 16, y = 40, h = 5, w = 11, class = "mud", bounds = { w = 11, h = 5, x = 0, y = 0 } } }
            , { description = "", entity = { id = 5, collidable = True, x = 48, y = 40, h = 80, w = 80, class = "tree", bounds = { w = 13, h = 12, x = 28, y = 29 } } }
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
