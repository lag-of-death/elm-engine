module Update exposing (update)

import Collisions exposing (..)
import Helpers exposing (..)
import Random exposing (..)
import Task exposing (..)
import Time exposing (..)
import Types exposing (..)


onTick model items player =
    let
        player_ =
            toPlayer_ player

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


onKeyDown model player entity itemsAndEnemies keyCode =
    let
        player_ =
            toPlayer_ player

        closeEnemies =
            List.filter (\enemy -> not (isPlayerAway player_.entity enemy.entity 30)) model.enemies
    in
    case keyCode of
        "ArrowRight" ->
            ( { model
                | player =
                    { player
                        | entity =
                            { entity
                                | x =
                                    if List.any (\item -> isSthOnTheRight player_.entity item.entity) itemsAndEnemies then
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
                                    if List.any (\item -> isSthAbove player_.entity item.entity) itemsAndEnemies then
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
                                    if List.any (\item -> isSthBeneath player_.entity item.entity) itemsAndEnemies then
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
                                    if List.any (\item -> isSthOnTheLeft player_.entity item.entity) itemsAndEnemies then
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
        itemsWithPlayer =
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
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | x = itemEntity.x + 1 } } ]

    else if
        shouldEnemyMoveDown player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthBeneath itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y + 1 } } ]

    else if
        shouldEnemyMoveUp player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthAbove itemEntity entity)
                    itemsWithPlayer
                )
    then
        [ { item | entity = { itemEntity | y = itemEntity.y - 1 } } ]

    else if
        shouldEnemyMoveLeft player.chase playerEntity itemEntity
            && not
                (List.any
                    (\{ entity } -> isSthOnTheLeft itemEntity entity)
                    itemsWithPlayer
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
        itemsAndEnemies =
            concatEntities model.enemies model.items

        player =
            model.player

        entity =
            player.entity

        items =
            model.items
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
            in
            onTick model items { player | chase = chase_ }

        Second _ ->
            ( model, Random.generate NewRandomNumber (Random.int 0 6) )

        Tick _ ->
            onTick model items player

        Walk dir ->
            onKeyDown model player entity itemsAndEnemies dir

        SetDirection dir ->
            let
                class_ =
                    if not (String.contains "Arrow" dir) then
                        player.entity.class ++ " " ++ "character--stopped"

                    else
                        player.entity.class
            in
            ( { model | player = { player | direction = dir, entity = { entity | class = class_ } } }, Cmd.none )

        KeyDown keyCode ->
            onKeyDown model player entity itemsAndEnemies keyCode
