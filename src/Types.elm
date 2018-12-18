module Types exposing (Enemy, Entity, Item, Model, Msg(..), Player)

import Time



--TODO: introduce multiple bounds - List Bound, this way a single image could have multiple collidable places
--For now there's only one collidable tree in the woods (and there are 4 trees, so there should be 4 such places)


type alias Entity =
    { x : Int
    , y : Int
    , h : Int
    , w : Int
    , class : String
    , collidable : Bool
    , id : Int
    , bounds : Bound
    }


type alias Bound =
    { w : Int, h : Int, x : Int, y : Int }


type alias Player =
    { hp : Int, exp : Int, action : String, direction : String, v : Int, r : Int, closeEnemies : List Enemy, entity : Entity, chase : { x : Int, y : Int } }


type alias Item =
    { entity : Entity }


type alias Enemy =
    { entity : Entity }


type alias Model =
    { player : Player
    , items : List Item
    , enemies : List Enemy
    }


type Msg
    = KeyDown String
    | Tick Time.Posix
    | Second Time.Posix
    | NewRandomNumber Int
    | SetDirection String
    | Walk String
    | Fight
    | SetAction String
    | EnemyFight Int
