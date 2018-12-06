module Update exposing (update)

import Collisions exposing (..)
import Helpers exposing (..)
import Types exposing (..)


updateEnemies oldEnemies closeItems newEnemies player =
    let
        itemsWithPlayer =
            List.append closeItems (toEntities [ player ])

        enemy =
            Maybe.withDefault
                defaultEnemy
                (List.head oldEnemies)

        closeEnemyAsArray =
            getUpdatedEnemy player enemy (List.concat [ itemsWithPlayer, toEntities oldEnemies, toEntities newEnemies.enemies_ ])

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
        updateEnemies (List.drop 1 oldEnemies) itemsWithPlayer newEnemies_ player


getUpdatedEnemy player item itemsWithPlayer =
    let
        playerEntity =
            player.entity

        itemEntity =
            item.entity
    in
    if itemEntity.id == -1 then
        []

    else if
        shouldEnemyMoveRight playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthOnTheRight itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | x = itemEntity.x + 1 } } ]

    else if
        shouldEnemyMoveDown playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthBeneath itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y + 1 } } ]

    else if
        shouldEnemyMoveUp playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthAbove itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y - 1 } } ]

    else if
        shouldEnemyMoveLeft playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthOnTheLeft itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | x = itemEntity.x - 1 } } ]

    else
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        everything =
            concatEntities model.enemies model.items

        player =
            model.player

        entity =
            player.entity

        player_ =
            { player
                | entity =
                    { entity | class = entity.class, x = player.r - entity.x, y = player.r - entity.y }
            }

        items =
            model.items
    in
    case msg of
        Tick e ->
            let
                closeItems_ =
                    List.filter (\item -> not (isPlayerAway player_.entity item.entity 50)) items

                { enemies_, closeOnes } =
                    updateEnemies model.enemies (toEntities closeItems_) { enemies_ = [], closeOnes = [] } player_
            in
            ( { model
                | enemies = enemies_
                , player =
                    { player
                        | closeEnemies = closeOnes
                    }
              }
            , Cmd.none
            )

        KeyDown a ->
            let
                closeEnemies =
                    List.filter (\enemy -> not (isPlayerAway player_.entity enemy.entity 30)) model.enemies
            in
            case a of
                "ArrowRight" ->
                    ( { model
                        | player =
                            { player
                                | entity =
                                    { entity
                                        | x =
                                            if List.any (\item -> isSthOnTheRight player_.entity item.entity) everything then
                                                player.entity.x

                                            else
                                                player.entity.x - player.v
                                        , class = "character--going-right"
                                    }
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowUp" ->
                    ( { model
                        | player =
                            { player
                                | entity =
                                    { entity
                                        | y =
                                            if List.any (\item -> isSthAbove player_.entity item.entity) everything then
                                                player.entity.y

                                            else
                                                player.entity.y + player.v
                                        , class = "character--going-up"
                                    }
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowDown" ->
                    ( { model
                        | player =
                            { player
                                | entity =
                                    { entity
                                        | y =
                                            if List.any (\item -> isSthBeneath player_.entity item.entity) everything then
                                                player.entity.y

                                            else
                                                player.entity.y - player.v
                                        , class = "character--going-down"
                                    }
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( { model
                        | player =
                            { player
                                | entity =
                                    { entity
                                        | x =
                                            if List.any (\item -> isSthOnTheLeft player_.entity item.entity) everything then
                                                player.entity.x

                                            else
                                                player.entity.x + player.v
                                        , class = "character--going-left"
                                    }
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
