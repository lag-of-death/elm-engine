module Collisions exposing (isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight)

import Types exposing (..)


isSthOnTheRight : Player -> Item -> Bool
isSthOnTheRight player item =
    if (player.r + player.w - player.x) >= item.x then
        if (player.r + player.h - player.y) <= item.y then
            False

        else if (player.r - player.y) >= item.y + item.h then
            False

        else if (player.r - player.x) >= item.x + item.w then
            False

        else
            item.collidable

    else
        False


isSthAbove : Player -> Item -> Bool
isSthAbove player item =
    let
        x1 =
            player.r - player.x

        y1 =
            player.r - player.y

        x2 =
            item.x

        y2 =
            item.y

        w1 =
            player.w

        w2 =
            item.w

        collidingFromRight =
            (x2 + w2 + w1) > (x1 + w1)

        collidingByY =
            y1 == (y2 + item.h)

        collidingFromLeft =
            (x1 + w1) > x2
    in
    item.collidable && collidingFromLeft && collidingByY && collidingFromRight


isSthBeneath : Player -> Item -> Bool
isSthBeneath player item =
    let
        x1 =
            player.r - player.x

        y1 =
            player.r - player.y

        x2 =
            item.x

        y2 =
            item.y

        w1 =
            player.w

        w2 =
            item.w

        collidingFromRight =
            (x2 + w2 + w1) > (x1 + w1)

        collidingByY =
            (y1 + player.h) == y2

        collidingFromLeft =
            (x1 + w1) > x2
    in
    item.collidable && collidingFromLeft && collidingByY && collidingFromRight


isSthOnTheLeft : Player -> Item -> Bool
isSthOnTheLeft player item =
    if (player.r - player.x) <= item.x + item.w then
        if (player.r + player.h - player.y) <= item.y then
            False

        else if (player.r - player.y) >= item.y + item.h then
            False

        else if (player.r - player.x + player.w) <= item.x then
            False

        else
            item.collidable

    else
        False
