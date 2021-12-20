-- A simple yahtzee game


module Main exposing (Model, Msg(..), init, main, rollDice, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events exposing (..)
import List
import Random
import Random.Extra
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
    }


type alias Dice =
    { value : Int, held : Bool }


type alias Player =
    { name : String
    , scoreboard : Dict String (Maybe Int)
    }


emptyScoreboard : Dict String (Maybe Int)
emptyScoreboard =
    Dict.fromList
        [ ( "ones", Nothing )
        , ( "twos", Nothing )
        , ( "threes", Nothing )
        , ( "fours", Nothing )
        , ( "fives", Nothing )
        , ( "sixes", Nothing )
        , ( "sumUpper", Nothing )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Array.repeat 5 (Dice 1 False)) [ Player "Player 1" emptyScoreboard ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewDice (List Dice)
    | ToggleHold Int


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
            Dice 1 False

        Just die ->
            Dice die.value (not die.held)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (viewDice model.dice)
        , h1 [] [ text (viewDiceAsText model.dice) ]
        , button [ onClick Roll ] [ text "Roll" ]
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
        , viewBoard
        ]


viewBoard : Html Msg
viewBoard =
    table []
        [ tr []
            [ th [] []
            , th [] [ text "Players" ]
            , th [] [ text "Player 1" ]
            ]
        , tr []
            [ td [] [ button [] [ text "Score" ] ]
            , td [] [ text "Ones" ]
            , td [] [ text "(score)" ]
            ]
        , tr []
            [ td [] []
            , td [] [ text "Twos" ]
            , td [] [ text "(score)" ]
            ]
        , tr []
            [ td [] []
            , td [] [ text "Threes" ]
            , td [] [ text "(score)" ]
            ]
        , tr []
            [ td [] []
            , td [] [ text "Fours" ]
            , td [] [ text "(score)" ]
            ]
        , tr []
            [ td [] []
            , td [] [ text "Fives" ]
            , td [] [ text "(score)" ]
            ]
        , tr []
            [ td [] []
            , td [] [ text "Sixes" ]
            , td [] [ text "(score)" ]
            ]
        ]


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
