-- A simple yahtzee game


module Main exposing (Model, Msg(..), init, main, rollDice, subscriptions, update, view)

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



-- MODEL


type alias Model =
    { dice : Array Dice
    , players : List Player
    , activePlayer : Int
    }


type alias Dice =
    { value : Int, held : Bool }


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Array.repeat 5 (Dice 1 False))
        [ newPlayer "Player 1"
        , newPlayer "Player 2"
        ]
        0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewDice (List Dice)
    | ToggleHold Int
    | SelectAll
    | UnselectAll
    | Score Category


type Category
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewDice (rollDice model.dice)
            )

        NewDice newDice ->
            ( { model | dice = Array.fromList newDice }
            , Cmd.none
            )

        ToggleHold diceIndex ->
            ( { model | dice = toggleHold model diceIndex }
            , Cmd.none
            )

        SelectAll ->
            ( { model | dice = Array.map (\die -> { die | held = True }) model.dice }
            , Cmd.none
            )

        UnselectAll ->
            ( { model | dice = Array.map (\die -> { die | held = False }) model.dice }
            , Cmd.none
            )

        Score category ->
            ( { model
                | players = scorePlayer model category
                , activePlayer = nextPlayer model.activePlayer (List.length model.players)
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
                    scoreNumber model.dice 1
            in
            List.setAt model.activePlayer { player | ones = Just score, sum = player.sum + score } model.players

        Twos ->
            let
                score =
                    scoreNumber model.dice 2
            in
            List.setAt model.activePlayer { player | twos = Just score, sum = player.sum + score } model.players

        Threes ->
            let
                score =
                    scoreNumber model.dice 3
            in
            List.setAt model.activePlayer { player | threes = Just score, sum = player.sum + score } model.players

        Fours ->
            let
                score =
                    scoreNumber model.dice 4
            in
            List.setAt model.activePlayer { player | fours = Just score, sum = player.sum + score } model.players

        Fives ->
            let
                score =
                    scoreNumber model.dice 5
            in
            List.setAt model.activePlayer { player | fives = Just score, sum = player.sum + score } model.players

        Sixes ->
            let
                score =
                    scoreNumber model.dice 6
            in
            List.setAt model.activePlayer { player | sixes = Just score, sum = player.sum + score } model.players


scoreNumber : Array Dice -> Int -> Int
scoreNumber dice number =
    Array.toList dice
        |> List.map .value
        |> List.filter (\value -> value == number)
        |> List.sum


canHasPlayer : Maybe Player -> Player
canHasPlayer maybePlayer =
    case maybePlayer of
        Nothing ->
            newPlayer "unknown"

        Just player ->
            player


rollDice : Array Dice -> Random.Generator (List Dice)
rollDice dice =
    Array.toList dice |> List.map roll |> Random.Extra.sequence


roll : Dice -> Random.Generator Dice
roll die =
    if die.held then
        Random.map (\value -> Dice value True) (Random.constant die.value)

    else
        Random.map (\value -> Dice value False) (Random.int 1 6)


toggleHold : Model -> Int -> Array Dice
toggleHold model diceIndex =
    let
        die =
            Array.get diceIndex model.dice
    in
    Array.set diceIndex (canHasDice die) model.dice


canHasDice : Maybe Dice -> Dice
canHasDice maybeDie =
    case maybeDie of
        Nothing ->
            Dice 7 False

        Just die ->
            Dice die.value (not die.held)



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
        [ h1 [] [ text (activePlayer.name ++ "'s turn") ]
        , div [] (viewDice model.dice)
        , h1 [] [ text (viewDiceAsText model.dice) ]
        , button [ onClick Roll ] [ text "Roll" ]
        , button [ onClick SelectAll ] [ text "Hold all" ]
        , button [ onClick UnselectAll ] [ text "Hold none" ]
        , div []
            [ Svg.svg
                [ SvgA.viewBox "0 0 600 100"
                , SvgA.width "600"
                , SvgA.height "100"
                ]
                (dieOne 0 "black"
                    ++ dieTwo 100 "green"
                    ++ dieThree 200 "blue"
                    ++ dieFour 300 "deeppink"
                    ++ dieFive 400 "orange"
                    ++ dieSix 500 "brown"
                )
            ]
        , viewBoard model.players
        ]


viewBoard : List Player -> Html Msg
viewBoard players =
    table []
        [ tr []
            ([ th [] []
             , th [] [ text "Players" ]
             ]
                ++ List.map viewPlayerName players
            )
        , tr []
            ([ td [] [ button [ onClick (Score Ones) ] [ text "Score" ] ]
             , th [] [ text "Ones" ]
             ]
                ++ List.map viewScore (List.map .ones players)
            )
        , tr []
            ([ td [] [ button [ onClick (Score Twos) ] [ text "Score" ] ]
             , th [] [ text "Twos" ]
             ]
                ++ List.map viewScore (List.map .twos players)
            )
        , tr []
            ([ td [] [ button [ onClick (Score Threes) ] [ text "Score" ] ]
             , th [] [ text "Threes" ]
             ]
                ++ List.map viewScore (List.map .threes players)
            )
        , tr []
            ([ td [] [ button [ onClick (Score Fours) ] [ text "Score" ] ]
             , th [] [ text "Fours" ]
             ]
                ++ List.map viewScore (List.map .fours players)
            )
        , tr []
            ([ td [] [ button [ onClick (Score Fives) ] [ text "Score" ] ]
             , th [] [ text "Fives" ]
             ]
                ++ List.map viewScore (List.map .fives players)
            )
        , tr []
            ([ td [] [ button [ onClick (Score Sixes) ] [ text "Score" ] ]
             , th [] [ text "Sixes" ]
             ]
                ++ List.map viewScore (List.map .sixes players)
            )
        , tr []
            ([ td [] []
             , th [] [ text "Sum" ]
             ]
                ++ List.map viewSum (List.map .sum players)
            )
        ]


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


viewDiceAsText : Array Dice -> String
viewDiceAsText dice =
    Array.toList dice
        |> List.map (\aDie -> aDie.value)
        |> List.map String.fromInt
        |> List.intersperse ", "
        |> String.concat


viewDice : Array Dice -> List (Html Msg)
viewDice dice =
    Array.toIndexedList dice |> List.map viewDiceSpan


viewDiceSpan : ( Int, Dice ) -> Html Msg
viewDiceSpan ( dieIndex, die ) =
    Svg.svg
        [ onClick (ToggleHold dieIndex)
        , SvgA.viewBox "0 0 100 100"
        , SvgA.width "100"
        , SvgA.height "100"
        ]
        (pickDie die)


pickDie : Dice -> List (Svg Msg)
pickDie die =
    let
        color =
            if die.held then
                "blue"

            else
                "black"
    in
    case die.value of
        1 ->
            dieOne 0 color

        2 ->
            dieTwo 0 color

        3 ->
            dieThree 0 color

        4 ->
            dieFour 0 color

        5 ->
            dieFive 0 color

        6 ->
            dieSix 0 color

        _ ->
            [ dieBox 0 "red" ]



-- SVG ASSETS


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
