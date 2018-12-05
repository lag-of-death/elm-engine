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
    { id = 0, x = x, y = y, w = w, h = h, collidable = True, class = "" }


updateEnemies oldEnemies itemsWithPlayer newEnemies player =
    let
        enemy =
            Maybe.withDefault
                { id = -1, x = -1, y = -1, h = -1, w = -1, collidable = True, class = "" }
                (List.head oldEnemies)

        closeEnemyAsArray =
            getUpdatedEnemy player enemy (List.concat [ itemsWithPlayer, oldEnemies, newEnemies.enemies_ ])

        newEnemies_ =
            { enemies_ =
                List.concat
                    [ newEnemies.enemies_
                    , if List.isEmpty closeEnemyAsArray && not (enemy.id == -1) then
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


getUpdatedEnemy player_ item itemsWithPlayer =
    if item.id == -1 then
        []

    else if
        shouldEnemyMoveRight player_ item
            && not
                (List.any
                    (\item_ -> isSthOnTheRight item item_)
                    itemsWithPlayer
                )
    then
        [ { item | x = item.x + 1 } ]

    else if
        shouldEnemyMoveDown player_ item
            && not
                (List.any
                    (\item_ -> isSthBeneath item item_)
                    itemsWithPlayer
                )
    then
        [ { item | y = item.y + 1 } ]

    else if
        shouldEnemyMoveUp player_ item
            && not
                (List.any
                    (\item_ -> isSthAbove item item_)
                    itemsWithPlayer
                )
    then
        [ { item | y = item.y - 1 } ]

    else if
        shouldEnemyMoveLeft player_ item
            && not
                (List.any
                    (\item_ -> isSthOnTheLeft item item_)
                    itemsWithPlayer
                )
    then
        [ { item | x = item.x - 1 } ]

    else
        []


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

                { enemies_, closeOnes } =
                    updateEnemies model.enemies itemsWithPlayer { enemies_ = [], closeOnes = [] } player_
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
                                , class = "character--going-right"
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
                                , class = "character--going-up"
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
                                , class = "character--going-down"
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
                                , class = "character--going-left"
                                , closeEnemies = closeEnemies
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
