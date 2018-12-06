module Collisions exposing (isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight)

import Types exposing (..)


collidingFromAbove player item =
    (player.entity.y + player.entity.h) > item.entity.y


collidingFromBeneath player item =
    player.entity.y < (item.entity.y + item.entity.h)


collidingFromRight player item =
    (item.entity.x + item.entity.w + player.entity.w) > (player.entity.x + player.entity.w)


collidingFromLeft player item =
    (player.entity.x + player.entity.w) > item.entity.x


isSthOnTheRight player item =
    let
        collidingByX =
            (player.entity.x + player.entity.w) == item.entity.x
    in
    item.entity.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


isSthAbove player item =
    let
        collidingByY =
            player.entity.y == (item.entity.y + item.entity.h)
    in
    item.entity.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthBeneath player item =
    let
        collidingByY =
            (player.entity.y + player.entity.h) == item.entity.y
    in
    item.entity.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthOnTheLeft player item =
    let
        collidingByX =
            player.entity.x == item.entity.x + item.entity.w
    in
    item.entity.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item
