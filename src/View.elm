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

        pd =
            player.direction

        pa =
            player.action

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
          <|
            if player.hp > 0 && (not <| List.isEmpty model.enemies) then
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
                                , A.style "animation" <| List.foldr (++) "" <| List.intersperse "," item.entity.animations
                                ]
                                [ if item.entity.collidable then
                                    Html.div
                                        [ A.class "bounds"
                                        , A.style "top" <| intToPx item.entity.bounds.y
                                        , A.style "left" <| intToPx item.entity.bounds.x
                                        , A.style "width" <| intToPx item.entity.bounds.w
                                        , A.style "height" <| intToPx item.entity.bounds.h
                                        ]
                                        []

                                  else
                                    Html.span [] []
                                ]
                        )
                    <|
                        List.filter (\item -> not <| isPlayerAway { x = player.r - player.entity.x, y = player.r - player.entity.y } item.entity 160) (concatEntities model.enemies items)
                , Html.div
                    [ A.style "position" "absolute"
                    , A.style "left" "1px"
                    , A.style "top" "1px"
                    ]
                    [ Html.text <| "EXP: " ++ String.fromInt player.exp ++ " " ++ "HP: " ++ String.fromInt player.hp ]
                , Html.div
                    [ A.style "top" <| intToPx player.r
                    , A.style "left" <| intToPx player.r
                    , A.style "width" <| intToPx player.entity.w
                    , A.style "height" <| intToPx player.entity.h
                    , A.style "animation" <| List.foldr (++) "" <| List.intersperse "," <| player.entity.animations
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

            else if List.isEmpty model.enemies then
                [ Html.div [ A.style "text-align" "center" ] [ Html.text "YOU WIN" ] ]

            else
                [ Html.div [ A.style "text-align" "center" ] [ Html.text "GAME OVER" ] ]
        , Html.div
            [ A.class "mobile-keyboard"
            ]
            [ Html.div [ A.style "width" "40%" ]
                [ Html.div []
                    [ Html.button
                        [ E.on "touchstart" <| succeed (SetDirection "ArrowLeft")
                        , E.on "touchend" <| succeed (SetDirection "StopLeft")
                        , E.onMouseDown (SetDirection "ArrowLeft")
                        , E.onMouseUp (SetDirection "StopLeft")
                        , A.class "button"
                        ]
                        [ Html.span [] [ Html.text "<" ] ]
                    , Html.button
                        [ E.on "touchstart" <| succeed (SetDirection "ArrowRight")
                        , E.on "touchend" <| succeed (SetDirection "StopRight")
                        , E.onMouseDown (SetDirection "ArrowRight")
                        , E.onMouseUp (SetDirection "StopRight")
                        , A.class "button"
                        ]
                        [ Html.span [] [ Html.text ">" ] ]
                    ]
                , Html.div []
                    [ Html.button
                        [ E.on "touchstart" <| succeed (SetDirection "ArrowUp")
                        , E.on "touchend" <| succeed (SetDirection "StopUp")
                        , E.onMouseDown (SetDirection "ArrowUp")
                        , E.onMouseUp (SetDirection "StopUp")
                        , A.class "button"
                        ]
                        [ Html.span [] [ Html.text "^" ] ]
                    , Html.button
                        [ E.on "touchstart" <| succeed (SetDirection "ArrowDown")
                        , E.on "touchend" <| succeed (SetDirection "StopDown")
                        , E.onMouseDown (SetDirection "ArrowDown")
                        , E.onMouseUp (SetDirection "StopDown")
                        , A.class "button"
                        ]
                        [ Html.span [] [ Html.text "v" ] ]
                    ]
                ]
            , Html.div [ A.style "width" "40%" ]
                [ Html.button
                    [ E.on "touchstart" <| succeed (SetAction "Attack")
                    , E.on "touchend" <| succeed (SetAction "StopAttack")
                    , E.onMouseDown (SetAction "Attack")
                    , E.onMouseUp (SetAction "StopAttack")
                    , A.class "button"
                    , A.style "width" "100%"
                    ]
                    [ Html.text "A" ]
                ]
            ]
        ]
    , title = ""
    }
