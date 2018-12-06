module Collisions exposing (collidingFromAbove, collidingFromBeneath, collidingFromLeft, collidingFromRight, isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight, shouldEnemyMoveDown, shouldEnemyMoveLeft, shouldEnemyMoveRight, shouldEnemyMoveUp)

import Helpers exposing (..)
import Types exposing (..)


collidingFromAbove player item =
    (player.y + player.h) > item.y


collidingFromBeneath player item =
    player.y < (item.y + item.h)


collidingFromRight player item =
    (item.x + item.w + player.w) > (player.x + player.w)


collidingFromLeft player item =
    (player.x + player.w) > item.x


isSthOnTheRight player item =
    let
        collidingByX =
            (player.x + player.w) == item.x
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


isSthAbove player item =
    let
        collidingByY =
            player.y == (item.y + item.h)
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthBeneath player item =
    let
        collidingByY =
            (player.y + player.h) == item.y
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthOnTheLeft player item =
    let
        collidingByX =
            player.x == item.x + item.w
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


shouldEnemyMoveRight player enemy =
    let
        diff =
            player.x - enemy.x
    in
    not (isPlayerAway player enemy 30) && (player.x > enemy.x)


shouldEnemyMoveDown player enemy =
    let
        diff =
            player.y - enemy.y
    in
    not (isPlayerAway player enemy 30) && player.y > enemy.y


shouldEnemyMoveLeft player enemy =
    let
        diff =
            enemy.x - player.x
    in
    not (isPlayerAway player enemy 30) && player.x < enemy.x


shouldEnemyMoveUp player enemy =
    let
        diff =
            enemy.y - player.y
    in
    not (isPlayerAway player enemy 30) && player.y < enemy.y
