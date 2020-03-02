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
import StartView exposing (viewStartInfo)



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
    ( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1" ]) Random.independentSeed )



--( Model End, Random.generate (GameStarted <| StartInfo [ "Player 1", "Player 2" ]) Random.independentSeed )
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

        StartTurnClicked playerIndex ->
            ( updateGameInfo (\gameInfo -> { gameInfo | roundState = PlayerInTurn playerIndex }) model, Cmd.none )

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

        AddToSharedPileClicked playerIndex selectedHandCardIndex ->
            ( updatePlayer playerIndex
                (\player ->
                    { player
                        | selectedHandCardIndex = Nothing
                        , hand = Array.Extra.removeAt selectedHandCardIndex player.hand
                        , cardToSharedPile = Array.get selectedHandCardIndex player.hand
                    }
                )
                model
            , Cmd.none
            )

        TakeSharedPileCardBackClicked playerIndex sharedPileCard ->
            ( updatePlayer playerIndex
                (\player ->
                    { player
                        | hand = Array.push sharedPileCard player.hand
                        , cardToSharedPile = Nothing
                    }
                )
                model
            , Cmd.none
            )

        EndTurnClicked playerIndex ->
            ( updateGameInfo
                (\gameInfo ->
                    Bool.Extra.ifElse (endRound <| endTurn playerIndex gameInfo) (endTurn playerIndex gameInfo) (playerIndex >= Array.length gameInfo.players - 1)
                )
                model
            , Cmd.none
            )

        RevealSharedPileCardClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    let
                        sharedPileCard =
                            Array.get (Debug.log "element: " (Array.length gameInfo.sharedPile - 1)) gameInfo.sharedPile
                    in
                    case sharedPileCard of
                        Just card ->
                            { gameInfo
                                | roundState = RevealSharedCardPlayerInTurn card 0
                                , sharedPile = Array.Extra.pop gameInfo.sharedPile
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
