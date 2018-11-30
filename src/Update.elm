module Update exposing (update)

import Collisions exposing (..)
import Types exposing (..)


isPlayerAway player enemy =
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
    x1 > 30 || x2 > 30 || y1 > 30 || y2 > 30


shouldEnemyMoveRight player enemy =
    let
        diff =
            player.x - enemy.x
    in
    not (isPlayerAway player enemy) && (player.x > enemy.x)


shouldEnemyMoveDown player enemy =
    let
        diff =
            player.y - enemy.y
    in
    not (isPlayerAway player enemy) && player.y > enemy.y


shouldEnemyMoveLeft player enemy =
    let
        diff =
            enemy.x - player.x
    in
    not (isPlayerAway player enemy) && player.x < enemy.x


shouldEnemyMoveUp player enemy =
    let
        diff =
            enemy.y - player.y
    in
    not (isPlayerAway player enemy) && player.y < enemy.y


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        player =
            model.player

        player_ =
            { player | x = player.r - player.x, y = player.r - player.y }

        items =
            model.items
    in
    case msg of
        Tick e ->
            let
                items_ =
                    Debug.log "ticking" <|
                        List.map
                            (\item ->
                                if item.class == "enemy" then
                                    if shouldEnemyMoveRight player_ item && not (List.any (\item_ -> isSthOnTheRight item item_) items) then
                                        { item | x = item.x + 1 }

                                    else if shouldEnemyMoveDown player_ item && not (List.any (\item_ -> isSthBeneath item item_) items) then
                                        { item | y = item.y + 1 }

                                    else if shouldEnemyMoveUp player_ item && not (List.any (\item_ -> isSthAbove item item_) items) then
                                        { item | y = item.y - 1 }

                                    else if shouldEnemyMoveLeft player_ item && not (List.any (\item_ -> isSthOnTheLeft item item_) items) then
                                        { item | x = item.x - 1 }

                                    else
                                        item

                                else
                                    item
                            )
                            items
            in
            ( { model
                | items = items_
              }
            , Cmd.none
            )

        KeyDown a ->
            let
                enemies =
                    List.filter (\item -> item.class == "enemy") items

                isPlayerAway_ =
                    List.any (\enemy -> not (isPlayerAway player_ enemy)) enemies
            in
            case a of
                "ArrowRight" ->
                    ( { model
                        | player =
                            { player
                                | x =
                                    if List.any (\item -> isSthOnTheRight player_ item) items then
                                        player.x

                                    else
                                        player.x - player.v
                                , appearance = "character--going-right"
                                , isAway = isPlayerAway_
                            }
                      }
                    , Cmd.none
                    )

                "ArrowUp" ->
                    ( { model
                        | player =
                            { player
                                | y =
                                    if List.any (\item -> isSthAbove player_ item) items then
                                        player.y

                                    else
                                        player.y + player.v
                                , appearance = "character--going-up"
                                , isAway = isPlayerAway_
                            }
                      }
                    , Cmd.none
                    )

                "ArrowDown" ->
                    ( { model
                        | player =
                            { player
                                | y =
                                    if List.any (\item -> isSthBeneath player_ item) items then
                                        player.y

                                    else
                                        player.y - player.v
                                , appearance = "character--going-down"
                                , isAway = isPlayerAway_
                            }
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( { model
                        | player =
                            { player
                                | x =
                                    if List.any (\item -> isSthOnTheLeft player_ item) items then
                                        player.x

                                    else
                                        player.x + player.v
                                , appearance = "character--going-left"
                                , isAway = isPlayerAway_
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
