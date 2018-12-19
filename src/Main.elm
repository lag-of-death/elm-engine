module Main exposing (main)

import Browser
import Browser.Events as E
import Collisions exposing (isSthNear)
import Helpers exposing (..)
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
    let
        { entity } =
            toPlayer model.player

        toId =
            \enemy -> enemy.entity.id

        nearEnemies =
            List.map toId <| List.filter (\item -> isSthNear entity item.entity) model.enemies

        awayButStillFightingEnemies =
            List.filter (\item -> not <| List.isEmpty item.entity.animations) model.enemies
    in
    Sub.batch
        [ if not (List.isEmpty nearEnemies && List.isEmpty awayButStillFightingEnemies) then
            Time.every 1000 (\_ -> EnemyFight nearEnemies)

          else
            Sub.none
        , if not (List.isEmpty model.player.closeEnemies) then
            Time.every 100 Tick

          else
            Sub.none
        , Time.every 1000 Second
        , if String.contains "Arrow" model.player.direction then
            Time.every 50 (\_ -> Walk model.player.direction)

          else
            Sub.none
        , Time.every 500 (\_ -> Fight model.player.action)
        ]



-- INIT


init : {} -> ( Model, Cmd Msg )
init flags =
    let
        enemies =
            [ { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 6 }, id = 0, x = 0, y = 0, h = 8, w = 10, class = "enemy_1", collidable = True } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 4, h = 8 }, id = 1, collidable = True, x = -10, y = 40, h = 10, w = 6, class = "enemy_1" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 2, collidable = True, x = -20, y = 40, h = 10, w = 10, class = "enemy_2" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 4 }, id = 3, collidable = True, x = -30, y = 40, h = 6, w = 10, class = "enemy_2" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 6, h = 8 }, id = 4, collidable = True, x = -40, y = 40, h = 10, w = 8, class = "enemy_2" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 6, h = 8 }, id = 5, collidable = True, x = -50, y = 40, h = 10, w = 8, class = "enemy_1" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 6, collidable = True, x = -60, y = 40, h = 10, w = 10, class = "enemy_1" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 8 }, id = 7, collidable = True, x = -70, y = 40, h = 10, w = 10, class = "enemy_2" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 12 }, id = 8, collidable = True, x = -80, y = 40, h = 14, w = 10, class = "enemy_1" } }
            , { entity = { animations = [], bounds = { x = 0, y = 0, w = 12, h = 8 }, id = 9, collidable = True, x = -90, y = 60, h = 10, w = 14, class = "enemy_2" } }
            ]

        closeEnemies =
            []

        player =
            { direction = "StopDown"
            , action = ""
            , chase = { x = 0, y = 0 }
            , closeEnemies = closeEnemies
            , v = 1
            , entity =
                { bounds = { w = 8, h = 10, x = 4, y = 5 }
                , x = 40
                , y = 30
                , h = 16
                , w = 16
                , class = ""
                , collidable = True
                , animations = [ "down 600ms steps(2) infinite" ]
                , id = 123
                }
            , r = 50
            , hp = 100
            , exp = 0
            }
    in
    ( { items =
            [ { entity = { animations = [], bounds = { x = 0, y = 0, w = 8, h = 4 }, id = 1, x = 36, y = 60, h = 6, w = 10, class = "boulder", collidable = True } }
            , { entity = { animations = [], id = 2, collidable = False, x = 16, y = 40, h = 5, w = 11, class = "mud", bounds = { w = 0, h = 0, x = 0, y = 0 } } }
            , { entity = { animations = [ "fire 2s steps(3) infinite" ], id = 3, collidable = True, x = 16, y = 70, h = 8, w = 8, class = "fire", bounds = { w = 8, h = 8, x = 0, y = 0 } } }
            , { entity = { animations = [], id = 5, collidable = True, x = 48, y = 40, h = 80, w = 80, class = "tree", bounds = { w = 13, h = 12, x = 28, y = 29 } } }
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
