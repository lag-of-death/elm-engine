module Types exposing (Item, Model, Msg(..), Player)

import Time


type alias Player =
    { x : Int, y : Int, h : Int, w : Int, v : Int, appearance : String, r : Int, closeEnemies : List Item }


type alias Item =
    { x : Int, y : Int, h : Int, w : Int, collidable : Bool, class : String, id : Int }


type alias Model =
    { player : Player
    , items : List Item
    , enemies : List Item
    }


type Msg
    = KeyDown String
    | Tick Time.Posix
