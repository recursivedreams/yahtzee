-- A simple yahtzee game


module Main exposing (main)

import Browser
import List.Extra as List
import Messages
    exposing
        ( GameSubMsg(..)
        , MenuSubMsg(..)
        , Msg(..)
        )
import Model
    exposing
        ( Category(..)
        , Die
        , DieFace(..)
        , GameState
        , MenuState
        , Model(..)
        , Player
        )
import Random
import Random.Extra as Random
import String
import View


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = View.view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Menu [ "" ]
    , Cmd.none
    )


testPlayer : String -> Int -> Int -> Player
testPlayer playerName oneTest sumTest =
    { name = playerName
    , id = 1234
    , ones = Just oneTest
    , twos = Nothing
    , threes = Nothing
    , fours = Nothing
    , fives = Nothing
    , sixes = Nothing
    , sum = sumTest
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Menu menuState ->
            updateMenu (Maybe.withDefault MenuNoOp (unwrapMenuMsg msg)) menuState

        Game gameState ->
            updateGame (Maybe.withDefault NoOp (unwrapGameMsg msg)) gameState

        Error errorMsg ->
            ( Error errorMsg, Cmd.none )


unwrapMenuMsg : Msg -> Maybe MenuSubMsg
unwrapMenuMsg msg =
    case msg of
        GameMsg _ ->
            Nothing

        MenuMsg subMsg ->
            Just subMsg


unwrapGameMsg : Msg -> Maybe GameSubMsg
unwrapGameMsg msg =
    case msg of
        MenuMsg _ ->
            Nothing

        GameMsg subMsg ->
            Just subMsg


updateMenu : MenuSubMsg -> MenuState -> ( Model, Cmd Msg )
updateMenu msg players =
    case msg of
        MenuNoOp ->
            ( Menu players, Cmd.none )

        UpdatePlayerName index name ->
            ( players
                |> List.setAt index name
                |> Menu
            , Cmd.none
            )

        RemovePlayer index ->
            ( players
                |> List.removeAt index
                |> Menu
            , Cmd.none
            )

        AddPlayer ->
            ( Menu <| players ++ [ "" ]
            , Cmd.none
            )

        StartGame ->
            ( startGame players, Cmd.none )


startGame : List String -> Model
startGame players =
    case players of
        [] ->
            Error "New Game: Need at least one player…"

        first :: rest ->
            Game
                { dice = List.repeat 5 (Die Blank False)
                , players = List.indexedMap Model.newPlayer players
                , activePlayer = 0
                , firstPlayerId = 0
                , restPlayerIds = List.range 1 (List.length rest)
                , activePlayerId = 0
                , waitingPlayerIds = List.range 1 (List.length rest)
                , rollsLeft = 3
                , newRound = True
                }


updateGame : GameSubMsg -> GameState -> ( Model, Cmd Msg )
updateGame msg gameState =
    case msg of
        NoOp ->
            ( Game gameState, Cmd.none )

        Roll ->
            ( Game gameState
            , Random.generate GameMsg (newDiceGenerator gameState.dice)
            )

        NewDice newDice ->
            ( Game
                { gameState
                    | dice = newDice
                    , rollsLeft = gameState.rollsLeft - 1
                    , newRound = False
                }
            , Cmd.none
            )

        ToggleHold diceIndex ->
            ( Game { gameState | dice = Model.toggleHold gameState diceIndex }
            , Cmd.none
            )

        SelectAll ->
            ( Game { gameState | dice = List.map (\die -> { die | held = True }) gameState.dice }
            , Cmd.none
            )

        UnselectAll ->
            ( Game { gameState | dice = List.map (\die -> { die | held = False }) gameState.dice }
            , Cmd.none
            )

        Score category ->
            ( Game
                { gameState
                    | dice = List.repeat 5 (Die Blank False)
                    , players = scorePlayer gameState category
                    , activePlayer = nextPlayer gameState.activePlayer (List.length gameState.players)
                    , activePlayerId = Maybe.withDefault gameState.firstPlayerId (List.head gameState.waitingPlayerIds)
                    , waitingPlayerIds = Maybe.withDefault gameState.restPlayerIds (List.tail gameState.waitingPlayerIds)
                    , rollsLeft = 3
                    , newRound = True
                }
            , Cmd.none
            )


nextPlayer : Int -> Int -> Int
nextPlayer playerIndex numPlayers =
    if playerIndex + 1 >= numPlayers then
        0

    else
        playerIndex + 1


scorePlayer : GameState -> Category -> List Player
scorePlayer game category =
    let
        player =
            canHasPlayer (List.getAt game.activePlayer game.players)

        scoreCategory score playa =
            if playa.id == game.activePlayerId then
                { player | ones = Just score, sum = playa.sum + score }

            else
                playa
    in
    case category of
        Ones ->
            let
                score =
                    scoreNumber game.dice One
            in
            List.map (scoreCategory score) game.players

        -- List.setAt game.activePlayer { player | ones = Just score, sum = player.sum + score } game.players
        Twos ->
            let
                score =
                    scoreNumber game.dice Two
            in
            List.setAt game.activePlayer { player | twos = Just score, sum = player.sum + score } game.players

        Threes ->
            let
                score =
                    scoreNumber game.dice Three
            in
            List.setAt game.activePlayer { player | threes = Just score, sum = player.sum + score } game.players

        Fours ->
            let
                score =
                    scoreNumber game.dice Four
            in
            List.setAt game.activePlayer { player | fours = Just score, sum = player.sum + score } game.players

        Fives ->
            let
                score =
                    scoreNumber game.dice Five
            in
            List.setAt game.activePlayer { player | fives = Just score, sum = player.sum + score } game.players

        Sixes ->
            let
                score =
                    scoreNumber game.dice Six
            in
            List.setAt game.activePlayer { player | sixes = Just score, sum = player.sum + score } game.players


scoreNumber : List Die -> DieFace -> Int
scoreNumber dice face =
    List.map .face dice
        |> List.filter (\dieFace -> dieFace == face)
        |> List.map Model.dieToInt
        |> List.sum


canHasPlayer : Maybe Player -> Player
canHasPlayer maybePlayer =
    case maybePlayer of
        Nothing ->
            Model.newPlayer 1234 "unknown"

        Just player ->
            player


newDiceGenerator : List Die -> Random.Generator GameSubMsg
newDiceGenerator dice =
    Random.map (\newDice -> NewDice newDice) (rollDice dice)


rollDice : List Die -> Random.Generator (List Die)
rollDice dice =
    List.map roll dice |> Random.sequence


roll : Die -> Random.Generator Die
roll die =
    if die.held then
        Random.map (\face -> Die face True) (Random.constant die.face)

    else
        Random.map (\face -> Die face False) (Random.uniform One [ Two, Three, Four, Five, Six ])
