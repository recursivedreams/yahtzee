module View exposing (view)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
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
        , Model(..)
        , Player
        )
import Svg exposing (Svg, g, svg)
import Svg.Attributes as SvgA


view : Model -> Html Msg
view model =
    case model of
        Menu players ->
            viewMenu players

        Game gameState ->
            viewGame gameState

        Error errorMsg ->
            div []
                [ h1 [] [ text "Error:" ]
                , h2 [] [ text errorMsg ]
                ]


viewMenu : List String -> Html Msg
viewMenu players =
    div []
        ([ h1 [] [ text "Yahtzee!" ] ]
            ++ List.indexedMap viewPlayerField players
            ++ [ div [] [ button [ onClick (MenuMsg AddPlayer) ] [ text "Add another player" ] ]
               , div [] [ button [ onClick (MenuMsg StartGame) ] [ text "Start game" ] ]
               ]
        )


viewPlayerField : Int -> String -> Html Msg
viewPlayerField index player =
    div []
        [ input
            [ A.type_ "text"
            , A.placeholder "Player name"
            , A.autofocus True
            , onInput (\text -> MenuMsg (UpdatePlayerName index text))
            ]
            []
        , button
            [ onClick (MenuMsg (RemovePlayer index))
            ]
            [ text "Remove" ]
        ]


viewGame : GameState -> Html Msg
viewGame gameState =
    let
        logPlayers =
            Debug.log "Players" (Debug.toString (gameState.firstPlayerId :: gameState.restPlayerIds))

        logActive =
            Debug.log "Active player" (Debug.toString gameState.activePlayerId)

        activePlayer =
            Maybe.withDefault (Model.newPlayer 1234 "unknown")
                (List.getAt gameState.activePlayer gameState.players)
    in
    div []
        [ div []
            [ svg
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
        , h1 [] [ text (activePlayer.name ++ "'s turn. Rolls left: " ++ String.fromInt gameState.rollsLeft) ]
        , div [] (viewDice gameState)
        , h1 [] [ text (viewDiceAsText gameState.dice) ]
        , button [ onClick (clickRoll gameState) ] [ text "Roll" ]
        , button [ onClick (clickSelectAll gameState) ] [ text "Hold all" ]
        , button [ onClick (GameMsg UnselectAll) ] [ text "Hold none" ]
        , viewBoard gameState activePlayer
        , div []
            [ svg
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
            ""

        Just score ->
            String.fromInt score


viewPlayerName : Player -> Html Msg
viewPlayerName player =
    th [] [ text player.name ]


viewDiceAsText : List Die -> String
viewDiceAsText dice =
    List.map (\aDie -> aDie.face) dice
        |> List.map Model.dieToInt
        |> List.map String.fromInt
        |> List.intersperse ", "
        |> String.concat


viewDice : GameState -> List (Html Msg)
viewDice gameState =
    List.indexedMap Tuple.pair gameState.dice |> List.map (viewDiceSpan gameState)


viewDiceSpan : GameState -> ( Int, Die ) -> Html Msg
viewDiceSpan gameState ( dieIndex, die ) =
    svg
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
