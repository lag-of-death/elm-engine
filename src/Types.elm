module Types exposing (Item, Model, Msg(..), Player)

import Time


type alias Entity =
    { x : Int
    , y : Int
    , h : Int
    , w : Int
    , class : String
    }


type alias Player =
    { v : Int, r : Int, closeEnemies : List Item, entity : Entity }


type alias Item =
    { entity : Entity, collidable : Bool, id : Int }


type alias Model =
    { player : Player
    , items : List Item
    , enemies : List Item
    }


type Msg
    = KeyDown String
    | Tick Time.Posix
