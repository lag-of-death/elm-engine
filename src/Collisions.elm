module Collisions exposing (isSthAbove, isSthBeneath, isSthOnTheLeft, isSthOnTheRight)

import Types exposing (..)


isSthOnTheRight : Player -> Item -> Bool
isSthOnTheRight player item =
    if (player.r + player.w - player.x) >= item.x then
        if (player.r + player.h - player.y) <= item.y then
            False

        else if (player.r - player.y) >= item.y + item.h then
            False

        else if (player.r - player.x) >= item.x + item.w - player.v then
            False

        else
            item.collidable

    else
        False


isSthAbove : Player -> Item -> Bool
isSthAbove player item =
    if (player.r - player.y) >= item.y + item.h then
        if (player.r - player.x) >= (item.x + item.w) then
            False

        else if (player.r - player.x) + player.w <= item.x then
            False

        else if (player.r - player.y) >= item.y + item.h + player.v then
            False

        else
            item.collidable

    else
        False


isSthBeneath : Player -> Item -> Bool
isSthBeneath player item =
    if (player.r + player.h - player.y) <= item.y then
        if (player.r - player.x) >= (item.x + item.w) then
            False

        else if (player.r - player.x) + player.w <= item.x then
            False

        else if (player.r + player.h - player.y + player.v) <= item.y then
            False

        else
            item.collidable

    else
        False


isSthOnTheLeft : Player -> Item -> Bool
isSthOnTheLeft player item =
    if (player.r - player.x) <= item.x + item.w then
        if (player.r + player.h - player.y) <= item.y then
            False

        else if (player.r - player.y) >= item.y + item.h then
            False

        else if (player.r - player.x + player.w) <= item.x + player.v then
            False

        else
            item.collidable

    else
        False
