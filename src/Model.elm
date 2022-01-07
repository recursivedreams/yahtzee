module Model exposing
    ( Category(..)
    , Die
    , DieFace(..)
    , GameState
    , MenuState
    , Model(..)
    , Player
    , dieToInt
    , newPlayer
    , toggleHold
    )

import List.Extra as List


type Model
    = Menu MenuState
    | Game GameState
    | Error String


type alias MenuState =
    List String


type alias GameState =
    { dice : List Die
    , players : List Player
    , activePlayer : Int
    , firstPlayerId : Int
    , restPlayerIds : List Int
    , activePlayerId : Int
    , waitingPlayerIds : List Int
    , rollsLeft : Int
    , newRound : Bool
    }


newPlayer : Int -> String -> Player
newPlayer index name =
    { name = name
    , id = index
    , ones = Nothing
    , twos = Nothing
    , threes = Nothing
    , fours = Nothing
    , fives = Nothing
    , sixes = Nothing
    , sum = 0
    }


type alias Die =
    { face : DieFace, held : Bool }


type Category
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Blank


type alias Player =
    { name : String
    , id : Int
    , ones : Maybe Int
    , twos : Maybe Int
    , threes : Maybe Int
    , fours : Maybe Int
    , fives : Maybe Int
    , sixes : Maybe Int
    , sum : Int
    }


dieToInt : DieFace -> Int
dieToInt face =
    case face of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Blank ->
            0


toggleHold : GameState -> Int -> List Die
toggleHold game diceIndex =
    let
        die =
            List.getAt diceIndex game.dice
    in
    List.setAt diceIndex (toggleMaybeDie die) game.dice


toggleMaybeDie : Maybe Die -> Die
toggleMaybeDie maybeDie =
    case maybeDie of
        Nothing ->
            Die Blank False

        Just die ->
            Die die.face (not die.held)
