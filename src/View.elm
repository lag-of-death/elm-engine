module View exposing (view)

import Helpers exposing (..)
import Html
import Html.Attributes as A
import Types exposing (..)


intToPx : Int -> String
intToPx intValue =
    String.fromInt intValue ++ "px"


view : Model -> { body : List (Html.Html Msg), title : String }
view model =
    let
        player =
            model.player

        entity =
            player.entity

        items =
            model.items

        viewportWidth =
            player.r * 2 + player.entity.w

        viewportHeight =
            player.r * 2 + player.entity.h
    in
    { body =
        [ Html.div
            [ A.class "viewport"
            , A.style "width" <| intToPx viewportWidth
            , A.style "height" <| intToPx viewportHeight
            ]
            [ Html.div
                [ A.class "world"
                , A.style "width" <| intToPx (viewportWidth * 2)
                , A.style "height" <| intToPx (viewportHeight * 2)
                , A.style "left" <| intToPx player.entity.x
                , A.style "top" <| intToPx player.entity.y
                ]
              <|
                List.map
                    (\item ->
                        Html.div
                            [ A.class item.entity.class
                            , A.style "top" <| intToPx item.entity.y
                            , A.style "left" <| intToPx item.entity.x
                            , A.style "width" <| intToPx item.entity.w
                            , A.style "height" <| intToPx item.entity.h
                            ]
                            []
                    )
                <|
                    List.filter (\item -> not <| isPlayerAway { player | entity = { entity | w = player.entity.w, h = player.entity.h, x = player.r - player.entity.x, y = player.r - player.entity.y } } item (player.r + player.entity.w)) (List.concat [ model.enemies, items ])
            , Html.div
                [ A.style "top" <| intToPx player.r
                , A.style "left" <| intToPx player.r
                , A.style "width" <| intToPx player.entity.w
                , A.style "height" <| intToPx player.entity.h
                , A.class player.entity.class
                , A.class "character"
                ]
                []
            ]
        ]
    , title = ""
    }
