module View exposing (view)

import Helpers exposing (..)
import Html
import Html.Attributes as A
import Html.Events as E
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
                            [ Html.div
                                [ A.class "bounds"
                                , A.style "top" <| intToPx item.entity.bounds.y
                                , A.style "left" <| intToPx item.entity.bounds.x
                                , A.style "width" <| intToPx item.entity.bounds.w
                                , A.style "height" <| intToPx item.entity.bounds.h
                                ]
                                []
                            ]
                    )
                <|
                    List.filter (\item -> not <| isPlayerAway { x = player.r - player.entity.x, y = player.r - player.entity.y } item.entity 160) (concatEntities model.enemies items)
            , Html.div
                [ A.style "top" <| intToPx player.r
                , A.style "left" <| intToPx player.r
                , A.style "width" <| intToPx player.entity.w
                , A.style "height" <| intToPx player.entity.h
                , A.class player.entity.class
                , A.class "character"
                ]
                [ Html.div
                    [ A.class "bounds"
                    , A.style "top" <| intToPx player.entity.bounds.y
                    , A.style "left" <| intToPx player.entity.bounds.x
                    , A.style "width" <| intToPx player.entity.bounds.w
                    , A.style "height" <| intToPx player.entity.bounds.h
                    ]
                    []
                , Html.div
                    [ A.style "position" "absolute"
                    , A.style "left" <| intToPx player.chase.x
                    , A.style "top" <| intToPx player.chase.y
                    , A.style "border-radius" "360px"
                    , A.style "border" "1px solid forestgreen"
                    ]
                    []
                ]
            ]
        , Html.div
            [ A.style "transform" "scale(2)"
            , A.style "display" "flex"
            , A.style "justify-content" "space-around"
            , A.style "width" "36%"
            ]
            [ Html.button [ E.onClick (KeyDown "ArrowLeft") ] [ Html.text "<" ]
            , Html.button [ E.onClick (KeyDown "ArrowRight") ] [ Html.text ">" ]
            , Html.button [ E.onClick (KeyDown "ArrowUp") ] [ Html.text "^" ]
            , Html.button [ E.onClick (KeyDown "ArrowDown") ] [ Html.text "v" ]
            ]
        ]
    , title = ""
    }
