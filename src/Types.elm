module Types exposing (Enemy, Entity, Item, Model, Msg(..), Player)

import Time


type alias Entity =
    { x : Int
    , y : Int
    , h : Int
    , w : Int
    , class : String
    , collidable : Bool
    , id : Int
    }


type alias Player =
    { v : Int, r : Int, closeEnemies : List Enemy, entity : Entity }


type alias Item =
    { entity : Entity, description : String }


type alias Enemy =
    { entity : Entity, hp : Int }


type alias Model =
    { player : Player
    , items : List Item
    , enemies : List Enemy
    }


type Msg
    = KeyDown String
    | Tick Time.Posix
