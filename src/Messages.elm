module Messages exposing
    ( GameSubMsg(..)
    , MenuSubMsg(..)
    , Msg(..)
    )

import Model exposing (Category, Die)


type Msg
    = MenuMsg MenuSubMsg
    | GameMsg GameSubMsg


type MenuSubMsg
    = MenuNoOp
    | UpdatePlayerName Int String
    | RemovePlayer Int
    | AddPlayer
    | StartGame


type GameSubMsg
    = NoOp
    | Roll
    | NewDice (List Die)
    | ToggleHold Int
    | SelectAll
    | UnselectAll
    | Score Category
