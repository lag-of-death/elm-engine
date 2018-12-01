module Helpers exposing (isPlayerAway)


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
