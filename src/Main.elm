-- A simple yahtzee game


module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import List
import List.Extra as List
import Random
import Random.Extra as Random
import String
import Svg exposing (Svg, g, svg)
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
    ( Menu { players = [ createNewPlayer 0 ], idCount = 1 }
    , Cmd.none
    )


type Model
    = Menu MenuState
    | Game GameState
    | Error String


type alias MenuState =
    { players : List PlayerCreation
    , idCount : Int
    }


type alias PlayerCreation =
    { name : String, id : Int, isAdded : Bool }


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


createNewPlayer : Int -> PlayerCreation
createNewPlayer id =
    PlayerCreation "" id False



-- newGame : Model
-- newGame =
--     Game
--         { dice = List.repeat 5 (Die Blank False)
--         , players =
--             [ newPlayer "Player 1"
--             , newPlayer "Player 2"
--             ]
--         , activePlayer = 0
--         , rollsLeft = 3
--         , newRound = True
--         }


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
    , id : Int
    , ones : Maybe Int
    , twos : Maybe Int
    , threes : Maybe Int
    , fours : Maybe Int
    , fives : Maybe Int
    , sixes : Maybe Int
    , sum : Int
    }


newPlayer : ( String, Int ) -> Player
newPlayer ( name, id ) =
    { name = name
    , id = id
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
    , id = 1234
    , ones = Just oneTest
    , twos = Nothing
    , threes = Nothing
    , fours = Nothing
    , fives = Nothing
    , sixes = Nothing
    , sum = sumTest
    }


type Msg
    = MenuMsg MenuSubMsg
    | GameMsg GameSubMsg


type MenuSubMsg
    = MenuNoOp
    | UpdatePlayerName Int String
    | AddPlayer Int
    | MorePlayers
    | StartGame


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
updateMenu msg menuState =
    case msg of
        MenuNoOp ->
            ( Menu menuState, Cmd.none )

        UpdatePlayerName id text ->
            ( Menu { menuState | players = updatePlayerName id text menuState.players }, Cmd.none )

        AddPlayer id ->
            let
                addPlayer player =
                    if player.id == id then
                        { player | isAdded = True }

                    else
                        player
            in
            ( Menu { menuState | players = List.map addPlayer menuState.players }, Cmd.none )

        MorePlayers ->
            ( Menu
                { menuState
                    | players = List.append menuState.players [ createNewPlayer menuState.idCount ]
                    , idCount = menuState.idCount + 1
                }
            , Cmd.none
            )

        StartGame ->
            ( startGame menuState.players, Cmd.none )


updatePlayerName : Int -> String -> List PlayerCreation -> List PlayerCreation
updatePlayerName id text players =
    List.map (updateName id text) players


updateName : Int -> String -> PlayerCreation -> PlayerCreation
updateName id text player =
    if player.id == id then
        let
            log1 =
                log "input" player.name
        in
        { player | name = text }

    else
        player


startGame : List PlayerCreation -> Model
startGame players =
    List.filter (\player -> player.isAdded) players
        |> List.map (\{ name, id } -> ( name, id ))
        |> newGame


newGame : List ( String, Int ) -> Model
newGame playerData =
    let
        ( _, ids ) =
            List.unzip playerData
    in
    case ids of
        [] ->
            Error "New Game: Cannot find any players."

        first :: rest ->
            Game
                { dice = List.repeat 5 (Die Blank False)
                , players = List.map newPlayer playerData
                , activePlayer = 0
                , firstPlayerId = first
                , restPlayerIds = rest
                , activePlayerId = first
                , waitingPlayerIds = rest
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
            newPlayer ( "unknown", 1234 )

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
        Menu menuState ->
            viewMenu menuState

        Game gameState ->
            viewGame gameState

        Error errorMsg ->
            div []
                [ h1 [] [ text "Error:" ]
                , h2 [] [ text errorMsg ]
                ]


viewMenu : MenuState -> Html Msg
viewMenu menuState =
    div []
        ([ h1 [] [ text "Yahtzee!" ] ]
            ++ playerFields menuState
            ++ [ div [] [ button [ onClick (MenuMsg MorePlayers) ] [ text "More players" ], span [] [ text "Sometext" ] ]
               , div [] [ button [ onClick (MenuMsg StartGame) ] [ text "Start game" ] ]
               ]
        )


playerFields : MenuState -> List (Html Msg)
playerFields menuState =
    List.map updateNewPlayer menuState.players


updateNewPlayer : PlayerCreation -> Html Msg
updateNewPlayer player =
    if player.isAdded then
        div [] [ text player.name ]

    else
        div []
            [ input
                [ A.type_ "text"
                , A.placeholder "Player name"
                , A.autofocus True
                , onInput (\text -> MenuMsg (UpdatePlayerName player.id text))
                ]
                []
            , button
                [ onClick (MenuMsg (AddPlayer player.id))
                ]
                [ text "Add player" ]
            ]



-- if players == [] then
--     [ div []
--         [ input
--             [ A.type_ "text"
--             , A.placeholder "Player name"
--             , A.autofocus True
--             ]
--             []
--         , button
--             [ onClick (MenuMsg MenuNoOp)
--             ]
--             [ text "Add player" ]
--         ]
--     ]
-- else
--     [ div [] [ text "Something went wrong" ] ]


viewGame : GameState -> Html Msg
viewGame gameState =
    let
        logPlayers =
            log "Players" (Debug.toString (gameState.firstPlayerId :: gameState.restPlayerIds))

        logActive =
            log "Active player" (Debug.toString gameState.activePlayerId)

        activePlayer =
            Maybe.withDefault (newPlayer ( "unknown", 1234 ))
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
        |> List.map dieToint
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
