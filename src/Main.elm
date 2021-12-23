-- A simple yahtzee game


module Main exposing (..)

import Array exposing (Array, length)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events exposing (..)
import List
import List.Extra as List
import Random
import Random.Extra
import String exposing (fromInt)
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
    ( Model
        (List.repeat 5 (Die Blank False))
        [ newPlayer "Player 1"
        , newPlayer "Player 2"
        ]
        0
        3
        True
    , Cmd.none
    )



-- MODEL


type alias Model =
    { dice : List Die
    , players : List Player
    , activePlayer : Int
    , rollsLeft : Int
    , newRound : Bool
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



-- UPDATE


type Msg
    = NoOp
    | Roll
    | NewDice (List Die)
    | ToggleHold Int
    | SelectAll
    | UnselectAll
    | Score Category


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Roll ->
            ( model
            , Random.generate NewDice (rollDice model.dice)
            )

        NewDice newDice ->
            ( { model
                | dice = newDice
                , rollsLeft = model.rollsLeft - 1
                , newRound = False
              }
            , Cmd.none
            )

        ToggleHold diceIndex ->
            ( { model | dice = toggleHold model diceIndex }
            , Cmd.none
            )

        SelectAll ->
            ( { model | dice = List.map (\die -> { die | held = True }) model.dice }
            , Cmd.none
            )

        UnselectAll ->
            ( { model | dice = List.map (\die -> { die | held = False }) model.dice }
            , Cmd.none
            )

        Score category ->
            ( { model
                | dice = List.repeat 5 (Die Blank False)
                , players = scorePlayer model category
                , activePlayer = nextPlayer model.activePlayer (List.length model.players)
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


scorePlayer : Model -> Category -> List Player
scorePlayer model category =
    let
        player =
            canHasPlayer (List.getAt model.activePlayer model.players)
    in
    case category of
        Ones ->
            let
                score =
                    scoreNumber model.dice One
            in
            List.setAt model.activePlayer { player | ones = Just score, sum = player.sum + score } model.players

        Twos ->
            let
                score =
                    scoreNumber model.dice Two
            in
            List.setAt model.activePlayer { player | twos = Just score, sum = player.sum + score } model.players

        Threes ->
            let
                score =
                    scoreNumber model.dice Three
            in
            List.setAt model.activePlayer { player | threes = Just score, sum = player.sum + score } model.players

        Fours ->
            let
                score =
                    scoreNumber model.dice Four
            in
            List.setAt model.activePlayer { player | fours = Just score, sum = player.sum + score } model.players

        Fives ->
            let
                score =
                    scoreNumber model.dice Five
            in
            List.setAt model.activePlayer { player | fives = Just score, sum = player.sum + score } model.players

        Sixes ->
            let
                score =
                    scoreNumber model.dice Six
            in
            List.setAt model.activePlayer { player | sixes = Just score, sum = player.sum + score } model.players


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


rollDice : List Die -> Random.Generator (List Die)
rollDice dice =
    List.map roll dice |> Random.Extra.sequence


roll : Die -> Random.Generator Die
roll die =
    if die.held then
        Random.map (\face -> Die face True) (Random.constant die.face)

    else
        Random.map (\face -> Die face False) (Random.uniform One [ Two, Three, Four, Five, Six ])


toggleHold : Model -> Int -> List Die
toggleHold model diceIndex =
    let
        die =
            List.getAt diceIndex model.dice
    in
    List.setAt diceIndex (toggleMaybeDie die) model.dice


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
    let
        activePlayer =
            Maybe.withDefault (newPlayer "unknown")
                (List.getAt model.activePlayer model.players)
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
        , h1 [] [ text (activePlayer.name ++ "'s turn. Rolls left: " ++ String.fromInt model.rollsLeft) ]
        , div [] (viewDice model)
        , h1 [] [ text (viewDiceAsText model.dice) ]
        , button [ onClick (clickRoll model) ] [ text "Roll" ]
        , button [ onClick (clickSelectAll model) ] [ text "Hold all" ]
        , button [ onClick UnselectAll ] [ text "Hold none" ]
        , viewBoard model activePlayer
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


clickRoll : Model -> Msg
clickRoll model =
    if model.rollsLeft <= 0 then
        NoOp

    else
        Roll


clickSelectAll : Model -> Msg
clickSelectAll model =
    if model.newRound then
        NoOp

    else
        SelectAll


viewBoard : Model -> Player -> Html Msg
viewBoard model activePlayer =
    table []
        [ tr []
            ([ th [] []
             , th [] [ text "Players" ]
             ]
                ++ List.map viewPlayerName model.players
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.ones Ones) ] [ text "Score" ] ]
             , th [] [ text "Ones" ]
             ]
                ++ List.map viewScore (List.map .ones model.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.twos Twos) ] [ text "Score" ] ]
             , th [] [ text "Twos" ]
             ]
                ++ List.map viewScore (List.map .twos model.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.threes Threes) ] [ text "Score" ] ]
             , th [] [ text "Threes" ]
             ]
                ++ List.map viewScore (List.map .threes model.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.fours Fours) ] [ text "Score" ] ]
             , th [] [ text "Fours" ]
             ]
                ++ List.map viewScore (List.map .fours model.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.fives Fives) ] [ text "Score" ] ]
             , th [] [ text "Fives" ]
             ]
                ++ List.map viewScore (List.map .fives model.players)
            )
        , tr []
            ([ td [] [ button [ onClick (clickScoreButton model activePlayer.sixes Sixes) ] [ text "Score" ] ]
             , th [] [ text "Sixes" ]
             ]
                ++ List.map viewScore (List.map .sixes model.players)
            )
        , tr []
            ([ td [] []
             , th [] [ text "Sum" ]
             ]
                ++ List.map viewSum (List.map .sum model.players)
            )
        ]


clickScoreButton : Model -> Maybe Int -> Category -> Msg
clickScoreButton model maybeScore category =
    if maybeScore /= Nothing || model.newRound then
        NoOp

    else
        Score category


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


viewDice : Model -> List (Html Msg)
viewDice model =
    List.indexedMap Tuple.pair model.dice |> List.map (viewDiceSpan model)


viewDiceSpan : Model -> ( Int, Die ) -> Html Msg
viewDiceSpan model ( dieIndex, die ) =
    Svg.svg
        [ onClick (clickToggleHold dieIndex model.newRound)
        , SvgA.viewBox "0 0 100 100"
        , SvgA.width "100"
        , SvgA.height "100"
        ]
        (pickDie die)


clickToggleHold : Int -> Bool -> Msg
clickToggleHold dieIndex newRound =
    if newRound then
        NoOp

    else
        ToggleHold dieIndex


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
