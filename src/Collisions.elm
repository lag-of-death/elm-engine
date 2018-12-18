module Collisions exposing (collidingFromAbove, collidingFromBeneath, collidingFromLeft, collidingFromRight, isSthAbove, isSthBeneath, isSthNear, isSthOnTheLeft, isSthOnTheRight, shouldEnemyMoveDown, shouldEnemyMoveLeft, shouldEnemyMoveRight, shouldEnemyMoveUp)

import Helpers exposing (..)
import Types exposing (..)


collidingFromAbove player item =
    (player.y + player.bounds.y + player.bounds.h) > item.y + item.bounds.y


collidingFromBeneath player item =
    player.y + player.bounds.y < (item.y + item.bounds.y + item.bounds.h)


collidingFromRight player item =
    (item.x + item.bounds.x + item.bounds.w + player.bounds.w) > (player.x + player.bounds.x + player.bounds.w)


collidingFromLeft player item =
    (player.bounds.x + player.x + player.bounds.w) > (item.bounds.x + item.x)


isSthNear player item =
    isSthOnTheRight player item
        || isSthAbove player item
        || isSthBeneath player item
        || isSthOnTheLeft player item


isSthOnTheRight player item =
    let
        collidingByX =
            (player.x + player.bounds.x + player.bounds.w) == (item.x + item.bounds.x)
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


isSthAbove player item =
    let
        collidingByY =
            player.y + player.bounds.y == (item.y + item.bounds.y + item.bounds.h)
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthBeneath player item =
    let
        collidingByY =
            (player.y + player.bounds.y + player.bounds.h) == item.y + item.bounds.y
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthOnTheLeft player item =
    let
        collidingByX =
            player.x + player.bounds.x == item.x + item.bounds.w + item.bounds.x
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


shouldEnemyMoveRight chase player enemy =
    not (isPlayerAway player enemy 30) && (player.x + chase.x > enemy.x)


shouldEnemyMoveDown chase player enemy =
    not (isPlayerAway player enemy 30) && (player.y + chase.y) > enemy.y


shouldEnemyMoveLeft chase player enemy =
    not (isPlayerAway player enemy 30) && (player.x + chase.x) < enemy.x


shouldEnemyMoveUp chase player enemy =
    not (isPlayerAway player enemy 30) && (player.y + chase.y) < enemy.y
