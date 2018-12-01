module Update exposing (update)

import Collisions exposing (..)
import Helpers exposing (..)
import Types exposing (..)


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


playerAsItem { x, y, h, w } =
    { id = 0, x = x, y = y, w = w, h = h, collidable = True, class = "", move = False }


updateEnemies oldEnemies allItems newEnemies player =
    let
        head =
            Maybe.withDefault { id = -999, x = -999, y = -999, h = -999, w = -999, collidable = True, class = "", move = False } (List.head oldEnemies)

        updatedHead =
            getUpdatedEnemy player head (List.concat [ allItems, oldEnemies, newEnemies ])

        newEnemies_ =
            List.concat [ newEnemies, [ updatedHead ] ]
    in
    if List.isEmpty oldEnemies then
        { enemies = List.filter (\x -> not <| x.id == -999) newEnemies_ }

    else
        updateEnemies (List.drop 1 oldEnemies) allItems newEnemies_ player


getUpdatedEnemy player_ item itemsWithPlayer =
    if
        shouldEnemyMoveRight player_ item
            && not
                (List.any
                    (\item_ -> isSthOnTheRight item item_)
                    itemsWithPlayer
                )
    then
        { item | x = item.x + 1, move = True }

    else if
        shouldEnemyMoveDown player_ item
            && not
                (List.any
                    (\item_ -> isSthBeneath item item_)
                    itemsWithPlayer
                )
    then
        { item | y = item.y + 1, move = True }

    else if
        shouldEnemyMoveUp player_ item
            && not
                (List.any
                    (\item_ -> isSthAbove item item_)
                    itemsWithPlayer
                )
    then
        { item | y = item.y - 1, move = True }

    else if
        shouldEnemyMoveLeft player_ item
            && not
                (List.any
                    (\item_ -> isSthOnTheLeft item item_)
                    itemsWithPlayer
                )
    then
        { item | x = item.x - 1, move = True }

    else
        { item | move = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        everything =
            List.concat [ model.enemies, model.items ]

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
                closeItems_ =
                    List.filter (\item -> not (isPlayerAway player_ item 50)) items

                playerItem =
                    playerAsItem player_

                itemsWithPlayer =
                    List.concat [ closeItems_, [ playerItem ] ]

                enemies_ =
                    .enemies <| updateEnemies model.enemies itemsWithPlayer [] player_

                closeEnemies =
                    List.filter (\enemy -> not (isPlayerAway player_ enemy 30)) enemies_
            in
            ( { model
                | enemies = enemies_
                , player =
                    { player
                        | closeEnemies = List.filter (\x -> x.move) closeEnemies
                    }
              }
            , Cmd.none
            )

        KeyDown a ->
            let
                closeEnemies =
                    List.filter (\enemy -> not (isPlayerAway player_ enemy 30)) model.enemies
            in
            case a of
                "ArrowRight" ->
                    ( { model
                        | player =
                            { player
                                | x =
                                    if List.any (\item -> isSthOnTheRight player_ item) everything then
                                        player.x

                                    else
                                        player.x - player.v
                                , appearance = "character--going-right"
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowUp" ->
                    ( { model
                        | player =
                            { player
                                | y =
                                    if List.any (\item -> isSthAbove player_ item) everything then
                                        player.y

                                    else
                                        player.y + player.v
                                , appearance = "character--going-up"
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowDown" ->
                    ( { model
                        | player =
                            { player
                                | y =
                                    if List.any (\item -> isSthBeneath player_ item) everything then
                                        player.y

                                    else
                                        player.y - player.v
                                , appearance = "character--going-down"
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( { model
                        | player =
                            { player
                                | x =
                                    if List.any (\item -> isSthOnTheLeft player_ item) everything then
                                        player.x

                                    else
                                        player.x + player.v
                                , appearance = "character--going-left"
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
