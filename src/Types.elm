module Types exposing (Item, Model, Msg(..), Player)

-- TYPES


type alias Player =
    { x : Int, y : Int, h : Int, w : Int, v : Int, appearance : String, r : Int }


type alias Item =
    { x : Int, y : Int, h : Int, w : Int, collidable : Bool }


type alias Model =
    { player : Player
    , items : List Item
    }


type Msg
    = KeyDown String
