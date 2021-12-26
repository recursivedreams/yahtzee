-- A simple yahtzee game


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events exposing (..)
import List
import List.Extra as List
import Random
import Random.Extra as Random
import String
import Svg exposing (Svg)
import Svg.Attributes as SvgA



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( newGame
    , Cmd.none
    )


type Model
    = Menu
    | Game GameState


type alias GameState =
    { dice : List Die
    , players : List Player
    , activePlayer : Int
    , rollsLeft : Int
    , newRound : Bool
    }


newGame : Model
newGame =
    Game
        { dice = List.repeat 5 (Die Blank False)
        , players =
            [ newPlayer "Player 1"
            , newPlayer "Player 2"
            ]
        , activePlayer = 0
        , rollsLeft = 3
        , newRound = True
        }


type alias Die =
    { face : DieFace, held : Bool }


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Blank


type Category
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes


type alias Player =
    { name : String
    , ones : Maybe Int
    , twos : Maybe Int
    , threes : Maybe Int
    , fours : Maybe Int
    , fives : Maybe Int
    , sixes : Maybe Int
    , sum : Int
    }


newPlayer : String -> Player
newPlayer playerName =
    { name = playerName
    , ones = Nothing
    , twos = Nothing
    , threes = Nothing
    , fours = Nothing
    , fives = Nothing
    , sixes = Nothing
    , sum = 0
    }


testPlayer : String -> Int -> Int -> Player
testPlayer playerName oneTest sumTest =
    { name = playerName
    , ones = Just oneTest
    , twos = Nothing
    , threes = Nothing
    , fours = Nothing
    , fives = Nothing
    , sixes = Nothing
    , sum = sumTest
    }


type Msg
    = MenuMsg
    | GameMsg GameSubMsg


type GameSubMsg
    = NoOp
    | Roll
    | NewDice (List Die)
    | ToggleHold Int
    | SelectAll
    | UnselectAll
    | Score Category


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Menu ->
            ( model, Cmd.none )

        Game gameState ->
            updateGame (Maybe.withDefault NoOp (unwrapGameMsg msg)) gameState


unwrapGameMsg : Msg -> Maybe GameSubMsg
unwrapGameMsg msg =
    case msg of
        MenuMsg ->
            Nothing

        GameMsg subMsg ->
            Just subMsg


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
            ( Game { gameState | dice = toggleHold gameState diceIndex }
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
    in
    case category of
        Ones ->
            let
                score =
                    scoreNumber game.dice One
            in
            List.setAt game.activePlayer { player | ones = Just score, sum = player.sum + score } game.players

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
        |> List.map dieToint
        |> List.sum


dieToint : DieFace -> Int
dieToint face =
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


canHasPlayer : Maybe Player -> Player
canHasPlayer maybePlayer =
    case maybePlayer of
        Nothing ->
            newPlayer "unknown"

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Menu ->
            div []
                [ button [] [ text "Start game" ] ]

        Game game ->
            let
                activePlayer =
                    Maybe.withDefault (newPlayer "unknown")
                        (List.getAt game.activePlayer game.players)
            in
            div []
                [ div []
                    [ Svg.svg
                        [ SvgA.viewBox "0 0 600 100"
                        , SvgA.width "600"
                        , SvgA.height "100"
                        ]
                        (dieOne 0 "purple"
                            ++ dieTwo 100 "green"
                            ++ dieThree 200 "blue"
                            ++ dieFour 300 "deeppink"
                            ++ dieFive 400 "orange"
                            ++ dieSix 500 "red"
                        )
                    ]
                , h1 [] [ text (activePlayer.name ++ "'s turn. Rolls left: " ++ String.fromInt game.rollsLeft) ]
                , div [] (viewDice game)
                , h1 [] [ text (viewDiceAsText game.dice) ]
                , button [ onClick (clickRoll game) ] [ text "Roll" ]
                , button [ onClick (clickSelectAll game) ] [ text "Hold all" ]
                , button [ onClick (GameMsg UnselectAll) ] [ text "Hold none" ]
                , viewBoard game activePlayer
                , div []
                    [ Svg.svg
                        [ SvgA.viewBox "0 0 600 100"
                        , SvgA.width "600"
                        , SvgA.height "100"
                        ]
                        (dieOne 0 "purple"
                            ++ dieTwo 100 "green"
                            ++ dieThree 200 "blue"
                            ++ dieFour 300 "deeppink"
                            ++ dieFive 400 "orange"
                            ++ dieSix 500 "red"
                        )
                    ]
                ]


clickRoll : GameState -> Msg
clickRoll gameState =
    if gameState.rollsLeft <= 0 then
        GameMsg NoOp

    else
        GameMsg Roll


clickSelectAll : GameState -> Msg
clickSelectAll gameState =
    if gameState.newRound then
        GameMsg NoOp

    else
        GameMsg SelectAll


viewBoard : GameState -> Player -> Html Msg
viewBoard gameState activePlayer =
    table []
        [ tr []
            ([ th [] []
             , th [] [ text "Players" ]
             ]
                ++ List.map viewPlayerName gameState.players
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.ones Ones) ] [ text "Score" ] ]
             , th [] [ text "Ones" ]
             ]
                ++ List.map viewScore (List.map .ones gameState.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.twos Twos) ] [ text "Score" ] ]
             , th [] [ text "Twos" ]
             ]
                ++ List.map viewScore (List.map .twos gameState.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.threes Threes) ] [ text "Score" ] ]
             , th [] [ text "Threes" ]
             ]
                ++ List.map viewScore (List.map .threes gameState.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.fours Fours) ] [ text "Score" ] ]
             , th [] [ text "Fours" ]
             ]
                ++ List.map viewScore (List.map .fours gameState.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.fives Fives) ] [ text "Score" ] ]
             , th [] [ text "Fives" ]
             ]
                ++ List.map viewScore (List.map .fives gameState.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton gameState activePlayer.sixes Sixes) ] [ text "Score" ] ]
             , th [] [ text "Sixes" ]
             ]
                ++ List.map viewScore (List.map .sixes gameState.players)
            )
        , tr []
            ([ td [] []
             , th [] [ text "Sum" ]
             ]
                ++ List.map viewSum (List.map .sum gameState.players)
            )
        ]


clickScoreButton : GameState -> Maybe Int -> Category -> Msg
clickScoreButton gameState maybeScore category =
    if maybeScore /= Nothing || gameState.newRound then
        GameMsg NoOp

    else
        GameMsg (Score category)


viewSum : Int -> Html Msg
viewSum sum =
    td [] [ text (String.fromInt sum) ]


viewScore : Maybe Int -> Html Msg
viewScore maybeScore =
    td [] [ text (viewScoreString maybeScore) ]


viewScoreString : Maybe Int -> String
viewScoreString maybeScore =
    case maybeScore of
        Nothing ->
            "(no score)"

        Just score ->
            String.fromInt score


viewPlayerName : Player -> Html Msg
viewPlayerName player =
    th [] [ text player.name ]


viewDiceAsText : List Die -> String
viewDiceAsText dice =
    List.map (\aDie -> aDie.face) dice
        |> List.map dieToint
        |> List.map String.fromInt
        |> List.intersperse ", "
        |> String.concat


viewDice : GameState -> List (Html Msg)
viewDice gameState =
    List.indexedMap Tuple.pair gameState.dice |> List.map (viewDiceSpan gameState)


viewDiceSpan : GameState -> ( Int, Die ) -> Html Msg
viewDiceSpan gameState ( dieIndex, die ) =
    Svg.svg
        [ onClick (clickToggleHold dieIndex gameState.newRound)
        , SvgA.viewBox "0 0 100 100"
        , SvgA.width "100"
        , SvgA.height "100"
        ]
        (pickDie die)


clickToggleHold : Int -> Bool -> Msg
clickToggleHold dieIndex newRound =
    if newRound then
        GameMsg NoOp

    else
        GameMsg (ToggleHold dieIndex)


pickDie : Die -> List (Svg Msg)
pickDie die =
    let
        color =
            if die.held then
                "blue"

            else
                "black"
    in
    case die.face of
        One ->
            dieOne 0 color

        Two ->
            dieTwo 0 color

        Three ->
            dieThree 0 color

        Four ->
            dieFour 0 color

        Five ->
            dieFive 0 color

        Six ->
            dieSix 0 color

        Blank ->
            dieBlank 0 "black"



-- SVG ASSETS


dieBlank : Int -> String -> List (Svg Msg)
dieBlank xpos color =
    [ dieBox (xpos + 5) color ]


dieOne : Int -> String -> List (Svg Msg)
dieOne xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 50) 50 color
    ]


dieTwo : Int -> String -> List (Svg Msg)
dieTwo xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 25) 25 color
    , dieDot (xpos + 75) 75 color
    ]


dieThree : Int -> String -> List (Svg Msg)
dieThree xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 25) 25 color
    , dieDot (xpos + 50) 50 color
    , dieDot (xpos + 75) 75 color
    ]


dieFour : Int -> String -> List (Svg Msg)
dieFour xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 25) 25 color
    , dieDot (xpos + 25) 75 color
    , dieDot (xpos + 75) 25 color
    , dieDot (xpos + 75) 75 color
    ]


dieFive : Int -> String -> List (Svg Msg)
dieFive xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 25) 25 color
    , dieDot (xpos + 25) 75 color
    , dieDot (xpos + 50) 50 color
    , dieDot (xpos + 75) 25 color
    , dieDot (xpos + 75) 75 color
    ]


dieSix : Int -> String -> List (Svg Msg)
dieSix xpos color =
    [ dieBox (xpos + 5) color
    , dieDot (xpos + 25) 25 color
    , dieDot (xpos + 25) 50 color
    , dieDot (xpos + 25) 75 color
    , dieDot (xpos + 75) 25 color
    , dieDot (xpos + 75) 50 color
    , dieDot (xpos + 75) 75 color
    ]


dieBox : Int -> String -> Svg Msg
dieBox x color =
    Svg.rect
        [ SvgA.x (String.fromInt x)
        , SvgA.y "5"
        , SvgA.width "90"
        , SvgA.height "90"
        , SvgA.fill "white"
        , SvgA.stroke color
        , SvgA.strokeWidth "4"
        , SvgA.rx "10"
        ]
        []


dieDot : Int -> Int -> String -> Svg Msg
dieDot x y color =
    Svg.circle
        [ SvgA.cx (String.fromInt x)
        , SvgA.cy (String.fromInt y)
        , SvgA.r "9"
        , SvgA.fill color
        ]
        []
