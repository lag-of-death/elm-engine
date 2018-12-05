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
            [ { id = 0, x = 0, y = 0, h = 6, w = 8, collidable = True, class = "enemy_1" }
            , { id = 1, x = -10, y = 40, h = 8, w = 8, collidable = True, class = "enemy_1" }
            , { id = 2, x = -20, y = 40, h = 6, w = 6, collidable = True, class = "enemy_2" }
            , { id = 3, x = -30, y = 40, h = 8, w = 8, collidable = True, class = "enemy_1" }
            , { id = 4, x = -40, y = 40, h = 4, w = 4, collidable = True, class = "enemy_2" }
            , { id = 5, x = -50, y = 40, h = 8, w = 8, collidable = True, class = "enemy_2" }
            , { id = 6, x = -60, y = 40, h = 6, w = 4, collidable = True, class = "enemy_1" }
            ]

        closeEnemies =
            []

        player =
            { closeEnemies = closeEnemies, v = 1, x = -10, y = -10, h = 16, w = 16, appearance = "character--going-down", r = 30 }
    in
    ( { items =
            [ { id = 0, x = 36, y = 30, h = 6, w = 13, collidable = True, class = "boulder" }
            , { id = 1, x = 36, y = 60, h = 4, w = 10, collidable = True, class = "boulder" }
            , { id = 2, x = 16, y = 40, h = 5, w = 11, collidable = False, class = "mud" }
            , { id = 3, x = 60, y = 80, h = 9, w = 11, collidable = False, class = "mud" }
            , { id = 4, x = 100, y = 40, h = 10, w = 11, collidable = True, class = "boulder" }
            , { id = 5, x = 70, y = 40, h = 8, w = 8, collidable = True, class = "fire" }
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
