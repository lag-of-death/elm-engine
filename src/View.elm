module View exposing (view)

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

        items =
            model.items

        viewportWidth =
            player.r * 2 + player.w

        viewportHeight =
            player.r * 2 + player.h
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
                , A.style "left" <| intToPx player.x
                , A.style "top" <| intToPx player.y
                ]
              <|
                List.concat
                    [ []
                    , List.map
                        (\item ->
                            Html.div
                                [ A.classList [ ( "boulder--collidable", item.class == "boulder" && item.collidable ), ( item.class, True ) ]
                                , A.style "top" <| intToPx item.y
                                , A.style "left" <| intToPx item.x
                                , A.style "width" <| intToPx item.w
                                , A.style "height" <| intToPx item.h
                                ]
                                []
                        )
                        items
                    ]
            , Html.div
                [ A.style "top" <| intToPx player.r
                , A.style "left" <| intToPx player.r
                , A.style "width" <| intToPx player.w
                , A.style "height" <| intToPx player.h
                , A.classList [ ( player.appearance, True ), ( "character", True ) ]
                ]
                []
            ]
        ]
    , title = ""
    }
