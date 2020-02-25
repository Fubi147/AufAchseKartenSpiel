module GameLogic exposing (..)

import Array
import Model exposing (..)
import Random exposing (Seed)


nextRound : GameInfo -> GameInfo
nextRound gameInfo =
    { gameInfo
        | roundState = NextPlayerInTurn 0
    }


endTurn : GameInfo -> GameInfo
endTurn gameInfo =
    let
        playerInTurn =
            case gameInfo.roundState of
                NextPlayerInTurn int ->
                    int

                PlayerInTurn int ->
                    int

                RevealSharedCard ->
                    0

                RevealSharedCardPlayerInTurn card int ->
                    --todo
                    0
    in
    { gameInfo
        | roundState = NextPlayerInTurn (playerInTurn + 1)
    }


endRound : GameInfo -> GameInfo
endRound gameInfo =
    { gameInfo | roundState = RevealSharedCard }


getCardValue : Card -> Int
getCardValue card =
    case card of
        Speed speed ->
            speed

        Minus50 ->
            -50

        _ ->
            0


isDiscard : Card -> Bool
isDiscard card =
    case card of
        Abwerfkarte ->
            True

        _ ->
            False


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
            Array.fromList <| List.indexedMap (\index name -> Player name 0 (takeCards index drawDeck) Array.empty Nothing Array.empty Nothing) startInfo.players
    in
    GameInfo players 0 (NextPlayerInTurn 0) Array.empty 0 newSeed
