module Update exposing (update)

import Collisions exposing (..)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown a ->
            let
                player =
                    model.player

                player_ =
                    { player | x = player.r - player.x, y = player.r - player.y }

                items =
                    model.items
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
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
