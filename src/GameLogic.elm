module GameLogic exposing (..)

import Array
import Flip exposing (flip)
import Maybe.Extra
import Model exposing (..)
import Random exposing (Seed)


nextRound : GameInfo -> GameInfo
nextRound gameInfo =
    { gameInfo
        | playerInTurn = gameInfo.playerInTurn + 1
        , cardAction = NoAction
        , cardToRoute = Nothing
        , players = addCardToPlayersRoute gameInfo.playerInTurn gameInfo.cardToRoute gameInfo.players
        , cardToSharedDeck = Nothing
        , sharedDeck = Maybe.Extra.unwrap gameInfo.sharedDeck (flip Array.push gameInfo.sharedDeck) gameInfo.cardToSharedDeck
    }


endTurn : GameInfo -> GameInfo
endTurn gameInfo =
    { gameInfo
        | playerInTurn = gameInfo.playerInTurn + 1
        , cardAction = NoAction
        , cardToRoute = Nothing
        , players = addCardToPlayersRoute gameInfo.playerInTurn gameInfo.cardToRoute gameInfo.players
        , cardToSharedDeck = Nothing
        , sharedDeck = Maybe.Extra.unwrap gameInfo.sharedDeck (flip Array.push gameInfo.sharedDeck) gameInfo.cardToSharedDeck
    }


endRound : GameInfo -> GameInfo
endRound gameInfo =
    { gameInfo | cardAction = RevealSharedCard }


getCardValue : Card -> Int
getCardValue card =
    case card of
        Speed speed ->
            speed

        Minus50 ->
            -50

        _ ->
            0


cardGenerator : Random.Generator Card
cardGenerator =
    Random.weighted
        ( 12, Tankstelle )
        [ ( 5, Minus50 )
        , ( 5, Nachziehkarte 1 )
        , ( 5, Nachziehkarte 2 )
        , ( 5, Abwerfkarte )
        , ( 4, Speed 10 )
        , ( 4, Speed 20 )
        , ( 5, Speed 30 )
        , ( 5, Speed 40 )
        , ( 6, Speed 50 )
        , ( 6, Speed 60 )
        , ( 7, Speed 70 )
        , ( 6, Speed 80 )
        , ( 6, Speed 90 )
        , ( 5, Speed 100 )
        , ( 5, Speed 110 )
        , ( 4, Speed 120 )
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
            Array.fromList <| List.indexedMap (\index name -> Player name 0 (takeCards index drawDeck) Array.empty) startInfo.players
    in
    GameInfo players 0 0 NoAction Nothing Nothing Array.empty 0 newSeed
