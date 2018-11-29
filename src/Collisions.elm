module Collisions exposing (isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight)

import Types exposing (..)


collidingFromAbove : Player -> Item -> Bool
collidingFromAbove player item =
    (player.y + player.h) > item.y


collidingFromBeneath : Player -> Item -> Bool
collidingFromBeneath player item =
    player.y < (item.y + item.h)


collidingFromRight : Player -> Item -> Bool
collidingFromRight player item =
    (item.x + item.w + player.w) > (player.x + player.w)


collidingFromLeft : Player -> Item -> Bool
collidingFromLeft player item =
    (player.x + player.w) > item.x


isSthOnTheRight : Player -> Item -> Bool
isSthOnTheRight player item =
    let
        collidingByX =
            (player.x + player.w) == item.x
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item


isSthAbove : Player -> Item -> Bool
isSthAbove player item =
    let
        collidingByY =
            player.y == (item.y + item.h)
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthBeneath : Player -> Item -> Bool
isSthBeneath player item =
    let
        collidingByY =
            (player.y + player.h) == item.y
    in
    item.collidable && collidingFromLeft player item && collidingByY && collidingFromRight player item


isSthOnTheLeft : Player -> Item -> Bool
isSthOnTheLeft player item =
    let
        collidingByX =
            player.x == item.x + item.w
    in
    item.collidable && collidingFromAbove player item && collidingByX && collidingFromBeneath player item
