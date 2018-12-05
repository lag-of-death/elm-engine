module Helpers exposing (isPlayerAway)


isPlayerAway player enemy r =
    let
        x1 =
            player.entity.x - enemy.entity.x

        x2 =
            enemy.entity.x - player.entity.x

        y1 =
            player.entity.y - enemy.entity.y

        y2 =
            enemy.entity.y - player.entity.y
    in
    x1 > r || x2 > r || y1 > r || y2 > r
