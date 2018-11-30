module Collisions exposing (isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight)

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
