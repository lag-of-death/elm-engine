module View exposing (view)

import Helpers exposing (..)
import Html
import Html.Attributes as A
import Html.Events as E
import Json.Decode exposing (..)
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
                    [ A.class "dot--to-be-chased"
                    , A.style "left" <| intToPx player.chase.x
                    , A.style "top" <| intToPx player.chase.y
                    ]
                    []
                ]
            ]
        , Html.div
            [ A.class "mobile-keyboard"
            ]
            [ Html.button
                [ E.on "touchstart" <| succeed (SetDirection "ArrowLeft")
                , E.on "touchend" <| succeed (SetDirection "none")
                , A.class "button"
                ]
                [ Html.span [] [ Html.text "<" ] ]
            , Html.button
                [ E.on "touchstart" <| succeed (SetDirection "ArrowRight")
                , E.on "touchend" <| succeed (SetDirection "none")
                , A.class "button"
                ]
                [ Html.span [] [ Html.text ">" ] ]
            , Html.button
                [ E.on "touchstart" <| succeed (SetDirection "ArrowUp")
                , E.on "touchend" <| succeed (SetDirection "none")
                , A.class "button"
                ]
                [ Html.span [] [ Html.text "^" ] ]
            , Html.button
                [ E.on "touchstart" <| succeed (SetDirection "ArrowDown")
                , E.on "touchend" <| succeed (SetDirection "none")
                , A.class "button"
                ]
                [ Html.span [] [ Html.text "v" ] ]
            ]
        ]
    , title = ""
    }
