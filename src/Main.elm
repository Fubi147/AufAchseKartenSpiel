module Main exposing (..)

import Array exposing (Array)
import Browser
import GameLogic exposing (..)
import Html exposing (..)
import Model exposing (..)
import Random exposing (Seed)
import StartView exposing (viewStartInfo)
import View exposing (..)



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
    ( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1", "Player 2" ]) Random.independentSeed )



--( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1" ]) Random.independentSeed )
-- UPDATE


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
                        | players = addCardToPlayersHand gameInfo.playerInTurn gameInfo.cardToRoute gameInfo.players
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
                        | players = addCardToPlayersHand gameInfo.playerInTurn gameInfo.cardToSharedDeck gameInfo.players
                        , cardToSharedDeck = Nothing
                    }
                )
                model
            , Cmd.none
            )

        EndTurn ->
            ( updateGameInfo
                (\gameInfo ->
                    if gameInfo.playerInTurn >= Array.length gameInfo.players - 1 then
                        endRound <| endTurn gameInfo

                    else
                        endTurn gameInfo
                )
                model
            , Cmd.none
            )

        RevealSharedDeckCardClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    let
                        sharedDeckCard =
                            Array.get (Array.length gameInfo.sharedDeck - 1) gameInfo.sharedDeck
                    in
                    case sharedDeckCard of
                        Just card ->
                            { gameInfo
                                | cardAction = ShowSharedCard card
                                , sharedDeck = Array.slice 0 -1 gameInfo.sharedDeck
                            }

                        Nothing ->
                            nextRound gameInfo
                )
                model
            , Cmd.none
            )



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
