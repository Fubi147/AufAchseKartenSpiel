module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Bool.Extra
import Browser
import GameLogic exposing (..)
import GameView exposing (..)
import Html exposing (..)
import Maybe.Extra
import Model exposing (..)
import Random exposing (Seed)
import StartView exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    --( Model <| Start <| StartInfo [ "Player 1" ], Cmd.none )
    --( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1", "Player 2", "Player 3" ]) Random.independentSeed )
    --( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1" ]) Random.independentSeed )
    --( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1", "Player 2" ]) Random.independentSeed )
    ( Model <| Start <| StartInfo [ "Player 1", "Player 2" ], Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( updateStartInfo (\startInfo -> StartInfo (startInfo.players ++ [ "Player " ++ (String.fromInt <| 1 + List.length startInfo.players) ])) model, Cmd.none )

        NameChanged index newName ->
            ( updateStartInfo (\startInfo -> StartInfo (List.indexedMap (\index2 name -> Bool.Extra.ifElse newName name (index2 == index)) startInfo.players)) model, Cmd.none )

        RemovePlayer index ->
            ( updateStartInfo (\startInfo -> StartInfo (List.take index startInfo.players ++ List.drop (index + 1) startInfo.players)) model, Cmd.none )

        StartGame startInfo ->
            ( model, Random.generate (GameStarted startInfo) Random.independentSeed )

        GameStarted startInfo seed ->
            ( Model <| Play <| initGame startInfo seed, Cmd.none )

        StartTurnClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    case gameInfo.roundState of
                        NextPlayerInTurn playerIndex ->
                            { gameInfo | roundState = PlayerInTurn playerIndex }

                        RevealSharedPileCardNextPlayerInTurn playerIndex ->
                            { gameInfo | roundState = RevealSharedPileCardPlayerInTurn playerIndex }

                        _ ->
                            gameInfo
                )
                model
            , Cmd.none
            )

        HandCardClicked playerIndex selectedHandCardIndex ->
            ( updatePlayer playerIndex (\player -> { player | selectedHandCardIndex = Just selectedHandCardIndex }) model, Cmd.none )

        AddToRouteClicked playerIndex selectedHandCardIndex ->
            ( updatePlayer playerIndex
                (\player ->
                    { player
                        | selectedHandCardIndex = Nothing
                        , hand = Array.Extra.removeAt selectedHandCardIndex player.hand
                        , cardsToRoute = Maybe.Extra.unwrap player.cardsToRoute (\card -> Array.push card player.cardsToRoute) (Array.get selectedHandCardIndex player.hand)
                    }
                )
                model
            , Cmd.none
            )

        TakeRouteCardBackClicked playerIndex routeCard ->
            ( updatePlayer playerIndex
                (\player ->
                    { player
                        | hand = Array.push routeCard player.hand
                        , cardsToRoute = Array.slice 0 -1 player.cardsToRoute
                    }
                )
                model
            , Cmd.none
            )

        AddToSharedPileClicked playerIndex ->
            ( model
                |> updateGameInfo (\gameInfo -> { gameInfo | sharedPileCard = getSelectedCardFromPlayersHand playerIndex gameInfo.players })
                |> updatePlayer playerIndex
                    (\player ->
                        { player
                            | selectedHandCardIndex = Nothing
                            , hand =
                                case player.selectedHandCardIndex of
                                    Just cardIndex ->
                                        Array.Extra.removeAt cardIndex player.hand

                                    Nothing ->
                                        player.hand
                        }
                    )
            , Cmd.none
            )

        TakeSharedPileCardBackClicked playerIndex sharedPileCard ->
            ( model
                |> updateGameInfo (\gameInfo -> { gameInfo | sharedPileCard = Nothing })
                |> updatePlayer playerIndex (\player -> { player | hand = Array.push sharedPileCard player.hand })
            , Cmd.none
            )

        EndTurnClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    case gameInfo.roundState of
                        PlayerInTurn playerIndex ->
                            if isLastPlayerInRound gameInfo then
                                startRoundEnd <| endTurn playerIndex gameInfo

                            else
                                endTurn playerIndex gameInfo

                        RevealSharedPileCardPlayerInTurn playerIndex ->
                            { gameInfo
                                | roundState =
                                    if isLastPlayerInRound gameInfo then
                                        RevealSharedPileCard

                                    else
                                        RevealSharedPileCardNextPlayerInTurn (modBy (Array.length gameInfo.players) (playerIndex + 1))
                                , players =
                                    updatePlayerInPlayers playerIndex
                                        (\player2 ->
                                            { player2
                                                | route = Array.append player2.route player2.cardsToRoute
                                                , selectedHandCardIndex = Nothing
                                                , cardsToRoute = Array.empty
                                            }
                                        )
                                        gameInfo.players
                            }

                        _ ->
                            gameInfo
                )
                model
            , Cmd.none
            )

        RevealSharedPileCardClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    let
                        sharedPileCard =
                            Array.get (Array.length gameInfo.sharedPile - 1) gameInfo.sharedPile

                        playersWithoutCardsToRoute =
                            Array.map (\player -> { player | cardsToRoute = Array.empty }) gameInfo.players
                    in
                    case sharedPileCard of
                        Just (DrawCard numberCardsToDraw) ->
                            { gameInfo
                                | roundState = RevealSharedPileCardNextPlayerInTurn (modBy (Array.length gameInfo.players) gameInfo.roundNumber)
                                , sharedPile = Array.Extra.pop gameInfo.sharedPile
                                , players = playersWithoutCardsToRoute
                                , sharedPileCard = Just (DrawCard numberCardsToDraw)
                            }

                        Just card ->
                            { gameInfo
                                | roundState = RevealSharedPileCard
                                , sharedPile = Array.Extra.pop gameInfo.sharedPile
                                , sharedPileSum = gameInfo.sharedPileSum + getCardValue card
                                , players =
                                    case card of
                                        Discard ->
                                            Array.map
                                                (\player ->
                                                    { player
                                                        | cardsToRoute = Array.slice -1 (Array.length player.route) player.route
                                                        , route = Array.slice 0 -1 player.route
                                                    }
                                                )
                                                playersWithoutCardsToRoute

                                        _ ->
                                            playersWithoutCardsToRoute
                                , sharedPileCard = Just card
                            }

                        Nothing ->
                            endRound { gameInfo | players = playersWithoutCardsToRoute, sharedPileCard = Nothing }
                )
                model
            , Cmd.none
            )

        NextStageClicked ->
            ( model |> updateGameInfo nextStage, Cmd.none )



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
