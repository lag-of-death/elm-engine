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
            [ { id = 0, entity = { x = 0, y = 0, h = 6, w = 8, class = "enemy_1" }, collidable = True }
            , { id = 1, collidable = True, entity = { x = -10, y = 40, h = 8, w = 8, class = "enemy_1" } }
            , { id = 2, collidable = True, entity = { x = -20, y = 40, h = 6, w = 6, class = "enemy_2" } }
            , { id = 3, collidable = True, entity = { x = -30, y = 40, h = 8, w = 8, class = "enemy_1" } }
            , { id = 4, collidable = True, entity = { x = -40, y = 40, h = 4, w = 4, class = "enemy_2" } }
            , { id = 5, collidable = True, entity = { x = -50, y = 40, h = 8, w = 8, class = "enemy_2" } }
            , { id = 6, collidable = True, entity = { x = -60, y = 40, h = 6, w = 4, class = "enemy_1" } }
            ]

        closeEnemies =
            []

        player =
            { closeEnemies = closeEnemies, v = 1, entity = { x = -10, y = -10, h = 16, w = 16, class = "character--going-down" }, r = 30 }
    in
    ( { items =
            [ { id = 0, entity = { x = 36, y = 30, h = 6, w = 13, class = "boulder" }, collidable = True }
            , { id = 1, entity = { x = 36, y = 60, h = 4, w = 10, class = "boulder" }, collidable = True }
            , { id = 2, collidable = False, entity = { x = 16, y = 40, h = 5, w = 11, class = "mud" } }
            , { id = 3, collidable = False, entity = { x = 60, y = 80, h = 9, w = 11, class = "mud" } }
            , { id = 4, collidable = True, entity = { x = 100, y = 40, h = 10, w = 11, class = "boulder" } }
            , { id = 5, collidable = True, entity = { x = 70, y = 40, h = 8, w = 8, class = "fire" } }
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
