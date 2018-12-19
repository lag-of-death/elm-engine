module Helpers exposing (concatEntities, defaultEnemy, isPlayerAway, toEntities, toPlayer)

import Types exposing (..)


toPlayer player =
    let
        entity =
            player.entity
    in
    { player
        | entity =
            { entity | x = player.r - entity.x, y = player.r - entity.y }
    }


isPlayerAway player enemy r =
    let
        x1 =
            player.x - enemy.x

        x2 =
            enemy.x - player.x

        y1 =
            player.y - enemy.y

        y2 =
            enemy.y - player.y
    in
    x1 > r || x2 > r || y1 > r || y2 > r


concatEntities : List { a | entity : Entity } -> List { b | entity : Entity } -> List { entity : Entity }
concatEntities a b =
    List.append (toEntities a) (toEntities b)


toEntities list =
    List.map (\x -> { entity = x.entity }) list


defaultEnemy : Enemy
defaultEnemy =
    { entity =
        { id = -1
        , x = -1
        , y = -1
        , h = -1
        , w = -1
        , class = ""
        , collidable = True
        , bounds = { w = -1, h = -1, y = -1, x = -1 }
        , animations = []
        }
    }
