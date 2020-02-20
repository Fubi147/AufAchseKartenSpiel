module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Random exposing (Seed)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { gameState : GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    --( Model <| Start <| StartInfo [ "Player 1" ], Cmd.none )
    ( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1", "Player 2", "Player 3" ]) Random.independentSeed )


type GameState
    = Start StartInfo
    | Play GameInfo
    | End


type alias StartInfo =
    { players : List String
    }


type alias GameInfo =
    { players : Array Player
    , stage : Int
    , playerInTurn : Int
    , cardAction : CardAction
    , cardToSharedDeck : Maybe Card
    , cardToRoute : Maybe Card
    , sharedDeck : Array Card
    , sharedDeckSum : Int
    , randomnessSeed : Seed
    }


type CardAction
    = NoAction
    | HandCardSelected Int


type alias Player =
    { name : String
    , hand : Array Card
    , route : Array Card
    }


type Card
    = Speed Int
    | Minus50
    | Tankstelle
    | Nachziehkarte Int
    | Abwerfkarte



-- UPDATE


type Msg
    = NameChanged Int String
    | RemovePlayer Int
    | AddPlayer
    | StartGame StartInfo
    | GameStarted StartInfo Seed
    | HandCardClicked Int
    | AddToSharedDeckClicked Int
    | TakeSharedCardBackClicked
    | AddToRouteClicked Int
    | TakeRouteCardBackClicked
    | EndTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( updateStartInfo (\startInfo -> StartInfo (startInfo.players ++ [ "Player " ++ (String.fromInt <| 1 + List.length startInfo.players) ])) model, Cmd.none )

        NameChanged index newName ->
            ( updateStartInfo
                (\startInfo ->
                    StartInfo
                        (List.indexedMap
                            (\index_ name ->
                                if index_ == index then
                                    newName

                                else
                                    name
                            )
                            startInfo.players
                        )
                )
                model
            , Cmd.none
            )

        RemovePlayer index ->
            ( updateStartInfo (\startInfo -> StartInfo (List.take index startInfo.players ++ List.drop (index + 1) startInfo.players)) model, Cmd.none )

        StartGame startInfo ->
            ( model, Random.generate (GameStarted startInfo) Random.independentSeed )

        GameStarted startInfo seed ->
            ( Model <| Play <| initGame startInfo seed, Cmd.none )

        HandCardClicked index ->
            ( updateGameInfo (\gameInfo -> { gameInfo | cardAction = HandCardSelected index }) model, Cmd.none )

        AddToRouteClicked selectedHandCardIndex ->
            ( updateGameInfo
                (\gameInfo ->
                    { gameInfo
                        | cardAction = NoAction
                        , players = removeCardFromPlayersHand gameInfo.playerInTurn selectedHandCardIndex gameInfo.players
                        , cardToRoute = getCardFromPlayersHand gameInfo.playerInTurn selectedHandCardIndex gameInfo.players
                    }
                )
                model
            , Cmd.none
            )

        TakeRouteCardBackClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    { gameInfo
                        | cardAction = NoAction
                        , players = addCardToPlayersHand gameInfo.playerInTurn gameInfo.cardToRoute gameInfo.players
                        , cardToRoute = Nothing
                    }
                )
                model
            , Cmd.none
            )

        AddToSharedDeckClicked selectedHandCardIndex ->
            ( updateGameInfo
                (\gameInfo ->
                    { gameInfo
                        | cardAction = NoAction
                        , players = removeCardFromPlayersHand gameInfo.playerInTurn selectedHandCardIndex gameInfo.players
                        , cardToSharedDeck = getCardFromPlayersHand gameInfo.playerInTurn selectedHandCardIndex gameInfo.players
                    }
                )
                model
            , Cmd.none
            )

        TakeSharedCardBackClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    { gameInfo
                        | cardAction = NoAction
                        , players = addCardToPlayersHand gameInfo.playerInTurn gameInfo.cardToSharedDeck gameInfo.players
                        , cardToSharedDeck = Nothing
                    }
                )
                model
            , Cmd.none
            )

        EndTurn ->
            ( model, Cmd.none )


addCardToPlayersHand : Int -> Maybe Card -> Array Player -> Array Player
addCardToPlayersHand playerIndex card players =
    case card of
        Just card2 ->
            Array.indexedMap
                (\playerIndex2 player ->
                    if playerIndex == playerIndex2 then
                        { player | hand = Array.push card2 player.hand }

                    else
                        player
                )
                players

        Nothing ->
            players


getCardFromPlayersHand : Int -> Int -> Array Player -> Maybe Card
getCardFromPlayersHand playerIndex cardIndex players =
    let
        currentPlayer =
            Array.get playerIndex players
    in
    Maybe.Extra.join <| Maybe.map (\player -> Array.get cardIndex player.hand) currentPlayer


removeCardFromPlayersHand : Int -> Int -> Array Player -> Array Player
removeCardFromPlayersHand playerIndex cardIndex players =
    case Array.get playerIndex players of
        Just a ->
            Array.set playerIndex { a | hand = Array.Extra.removeAt cardIndex a.hand } players

        Nothing ->
            players


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform (Speed 10)
        [ Minus50
        , Tankstelle
        , Nachziehkarte 1
        , Abwerfkarte
        ]


initGame : StartInfo -> Seed -> GameInfo
initGame startInfo seed =
    let
        numCards =
            10

        createDrawDeck =
            Random.list (numCards * List.length startInfo.players) cardGenerator

        ( drawDeck, newSeed ) =
            Random.step createDrawDeck seed

        takeCards index deck =
            Array.fromList <| List.take numCards <| List.drop (numCards * index) deck

        players =
            Array.fromList <| List.indexedMap (\index name -> Player name (takeCards index drawDeck) Array.empty) startInfo.players
    in
    GameInfo players 0 0 NoAction Nothing Nothing Array.empty 0 newSeed


updateStartInfo : (StartInfo -> StartInfo) -> Model -> Model
updateStartInfo updateFunction model =
    case model.gameState of
        Start startInfo ->
            { model | gameState = Start (updateFunction startInfo) }

        _ ->
            model


updateGameInfo : (GameInfo -> GameInfo) -> Model -> Model
updateGameInfo updateFunction model =
    case model.gameState of
        Play gameInfo ->
            { model | gameState = Play (updateFunction gameInfo) }

        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Start startInfo ->
            viewStartInfo startInfo

        Play gameInfo ->
            viewGame gameInfo

        End ->
            text "end"


viewStartInfo : StartInfo -> Html Msg
viewStartInfo startInfo =
    let
        mapPlayersToInput =
            \index name ->
                div []
                    [ text (String.fromInt (index + 1) ++ ". Player Name: ")
                    , input [ value name, onInput <| NameChanged index ] []
                    , button [ onClick (RemovePlayer index) ] [ text "Remove Player" ]
                    ]
    in
    div []
        [ div [] <| List.indexedMap mapPlayersToInput startInfo.players
        , button [ onClick AddPlayer ] [ text "Add Player" ]
        , button [ onClick <| StartGame startInfo ] [ text "Start Game" ]
        ]


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    let
        viewCards mf cards =
            div [] <| Array.toList <| Array.indexedMap mf cards

        showCardBack attributes =
            \_ _ -> button attributes [ text "X" ]

        viewPlayer playerIndex player =
            let
                showCardFront =
                    \cardIndex card ->
                        let
                            colorAttributes =
                                case gameInfo.cardAction of
                                    HandCardSelected selectedCardIndex ->
                                        if selectedCardIndex == cardIndex then
                                            [ style "background-color" "#73D216" ]

                                        else
                                            []

                                    _ ->
                                        []
                        in
                        button
                            ([ onClick (HandCardClicked cardIndex) ] ++ colorAttributes)
                            [ text <| Debug.toString card ]

                playerRouteCard =
                    gameInfo.cardToRoute
                        |> Maybe.map
                            (\card ->
                                div []
                                    [ button [ disabled True ] [ text <| Debug.toString card ]
                                    , button [ onClick TakeRouteCardBackClicked ] [ text "Take Card Back" ]
                                    ]
                            )
                        |> Maybe.withDefault (text "")
            in
            div []
                [ text <| player.name ++ ": "
                , div []
                    [ viewCards
                        (if playerIndex == gameInfo.playerInTurn then
                            showCardFront

                         else
                            showCardBack [ disabled True ]
                        )
                        player.hand
                    , viewCards (showCardBack [ disabled True ]) player.route
                    , if playerIndex == gameInfo.playerInTurn then
                        div []
                            [ playerRouteCard
                            , case gameInfo.cardAction of
                                HandCardSelected selectedHandCardIndex ->
                                    case gameInfo.cardToRoute of
                                        Nothing ->
                                            button [ onClick <| AddToRouteClicked selectedHandCardIndex ] [ text "Add To Route" ]

                                        _ ->
                                            text ""

                                _ ->
                                    text ""
                            ]

                      else
                        text ""
                    , div [] [ br [] [] ]
                    ]
                ]

        viewShared =
            let
                playerSharedCard =
                    gameInfo.cardToSharedDeck
                        |> Maybe.map
                            (\card ->
                                div []
                                    [ button [ disabled True ] [ text <| Debug.toString card ]
                                    , button [ onClick TakeSharedCardBackClicked ] [ text "Take Card Back" ]
                                    ]
                            )
                        |> Maybe.withDefault (text "")
            in
            div []
                [ text "Shared Deck:"
                , br [] []
                , text <| "Sum: " ++ String.fromInt gameInfo.sharedDeckSum
                , div []
                    [ viewCards (showCardBack [ disabled True ]) gameInfo.sharedDeck
                    , playerSharedCard
                    , case gameInfo.cardAction of
                        HandCardSelected selectedHandCardIndex ->
                            case gameInfo.cardToSharedDeck of
                                Nothing ->
                                    button [ onClick <| AddToSharedDeckClicked selectedHandCardIndex ] [ text "Add To Shared Deck" ]

                                _ ->
                                    text ""

                        _ ->
                            text ""
                    ]
                ]
    in
    div []
        [ div [] <| Array.toList <| Array.indexedMap viewPlayer gameInfo.players
        , viewShared
        , div [] [ br [] [] ]
        , button
            [ disabled (Maybe.Extra.isNothing gameInfo.cardToRoute || Maybe.Extra.isNothing gameInfo.cardToSharedDeck)
            , onClick EndTurn
            ]
            [ text "End Turn" ]
        ]
