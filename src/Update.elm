module Update exposing (update)

import Collisions exposing (..)
import Helpers exposing (..)
import Random exposing (..)
import Task exposing (..)
import Time exposing (..)
import Types exposing (..)


onTick enemies items player =
    let
        player_ =
            toPlayer_ player

        closeItems_ =
            List.filter (\item -> not (isPlayerAway player_.entity item.entity 50)) items

        { enemies_, closeOnes } =
            updateEnemies enemies (toEntities closeItems_) { enemies_ = [], closeOnes = [] } player_
    in
    { e = enemies_
    , p =
        { player
            | closeEnemies = closeOnes
        }
    }


onKeyDown enemies items player keyCode =
    let
        itemsAndEnemies =
            concatEntities enemies items

        playerEntity =
            player.entity

        { entity } =
            toPlayer_ player

        closeEnemies =
            List.filter (\enemy -> not (isPlayerAway entity enemy.entity 30)) enemies
    in
    case keyCode of
        "ArrowRight" ->
            { player
                | entity =
                    { playerEntity
                        | x =
                            if List.any (\item -> isSthOnTheRight entity item.entity) itemsAndEnemies then
                                player.entity.x

                            else
                                player.entity.x - player.v
                    }
                , closeEnemies = closeEnemies
            }

        "ArrowUp" ->
            { player
                | entity =
                    { playerEntity
                        | y =
                            if List.any (\item -> isSthAbove entity item.entity) itemsAndEnemies then
                                player.entity.y

                            else
                                player.entity.y + player.v
                    }
                , closeEnemies = closeEnemies
            }

        "ArrowDown" ->
            { player
                | entity =
                    { playerEntity
                        | y =
                            if List.any (\item -> isSthBeneath entity item.entity) itemsAndEnemies then
                                player.entity.y

                            else
                                player.entity.y - player.v
                    }
                , closeEnemies = closeEnemies
            }

        "ArrowLeft" ->
            { player
                | entity =
                    { playerEntity
                        | x =
                            if List.any (\item -> isSthOnTheLeft entity item.entity) itemsAndEnemies then
                                player.entity.x

                            else
                                player.entity.x + player.v
                    }
                , closeEnemies = closeEnemies
            }

        _ ->
            player


updateEnemies oldEnemies closeItems newEnemies player =
    let
        enemy =
            Maybe.withDefault
                defaultEnemy
                (List.head oldEnemies)

        closeEnemyAsArray =
            getUpdatedEnemy player enemy closeItems (toEntities oldEnemies) (toEntities newEnemies.enemies_)

        newEnemies_ =
            { enemies_ =
                List.concat
                    [ newEnemies.enemies_
                    , if List.isEmpty closeEnemyAsArray && not (enemy.entity.id == -1) then
                        [ enemy ]

                      else
                        closeEnemyAsArray
                    ]
            , closeOnes =
                List.concat
                    [ newEnemies.closeOnes
                    , closeEnemyAsArray
                    ]
            }
    in
    if List.isEmpty oldEnemies then
        newEnemies_

    else
        updateEnemies (List.drop 1 oldEnemies) closeItems newEnemies_ player


getUpdatedEnemy player item closeItems oldEnemies newEnemies =
    let
        itemsEnemiesAndPlayer =
            List.concat [ toEntities [ player ], closeItems, toEntities oldEnemies, toEntities newEnemies ]

        enemies =
            List.append oldEnemies newEnemies

        playerEntity =
            player.entity

        itemEntity =
            item.entity
    in
    if itemEntity.id == -1 then
        []

    else if
        shouldEnemyMoveRight player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthOnTheRight itemEntity entity)
                    itemsEnemiesAndPlayer
                )
    then
        [ { item | entity = { itemEntity | x = itemEntity.x + 1 } } ]

    else if
        shouldEnemyMoveDown player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthBeneath itemEntity entity)
                    itemsEnemiesAndPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y + 1 } } ]

    else if
        shouldEnemyMoveUp player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthAbove itemEntity entity)
                    itemsEnemiesAndPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y - 1 } } ]

    else if
        shouldEnemyMoveLeft player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthOnTheLeft itemEntity entity)
                    itemsEnemiesAndPlayer
                )
    then
        [ { item | entity = { itemEntity | x = itemEntity.x - 1 } } ]

    else
        []


toPlayer_ player =
    let
        entity =
            player.entity
    in
    { player
        | entity =
            { entity | x = player.r - entity.x, y = player.r - entity.y }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { player, items, enemies } =
            model
    in
    case msg of
        NewRandomNumber number ->
            let
                chase_ =
                    case number of
                        0 ->
                            { x = 12, y = 15 }

                        1 ->
                            { x = 2, y = 3 }

                        2 ->
                            { x = 2, y = 15 }

                        3 ->
                            { x = 12, y = 3 }

                        4 ->
                            { x = 6, y = -3 }

                        5 ->
                            { x = -4, y = 6 }

                        6 ->
                            { x = 12, y = 6 }

                        _ ->
                            { x = player.chase.x, y = player.chase.y }

                { e, p } =
                    onTick enemies items { player | chase = chase_ }
            in
            ( { model | enemies = e, player = p }, Cmd.none )

        Second _ ->
            ( model, Random.generate NewRandomNumber (Random.int 0 6) )

        Tick _ ->
            let
                { e, p } =
                    onTick enemies items player
            in
            ( { model | enemies = e, player = p }, Cmd.none )

        Fight ->
            let
                pe =
                    player.entity

                { entity } =
                    toPlayer_ player

                p =
                    { player
                        | exp =
                            if List.any (\item -> isSthOnTheRight entity item.entity) enemies then
                                player.exp + 1

                            else
                                player.exp
                    }
            in
            ( { model | player = p }, Cmd.none )

        Walk keyCode ->
            ( { model | player = onKeyDown enemies items player keyCode }, Cmd.none )

        SetDirection dir ->
            ( { model
                | player =
                    { player
                        | direction = dir
                    }
              }
            , Cmd.none
            )

        SetAction action ->
            ( { model
                | player =
                    { player
                        | action = action
                    }
              }
            , Cmd.none
            )

        KeyDown keyCode ->
            ( { model
                | player =
                    onKeyDown
                        enemies
                        items
                        player
                        keyCode
              }
            , Cmd.none
            )
