module GameLogic exposing (..)

import Array
import Maybe.Extra
import Model exposing (..)
import Random exposing (Seed)


isLastPlayerInRound : GameInfo -> Bool
isLastPlayerInRound gameInfo =
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.roundNumber - 1)

        PlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.roundNumber - 1)

        RevealSharedCardPlayerInTurn _ playerIndex _ ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.roundNumber - 1)

        _ ->
            False


nextRound : GameInfo -> GameInfo
nextRound gameInfo =
    { gameInfo
        | roundState = NextPlayerInTurn 0
    }
        |> fillPlayersHand


endTurn : Int -> GameInfo -> GameInfo
endTurn playerIndex gameInfo =
    let
        player =
            Array.get playerIndex gameInfo.players
    in
    { gameInfo
        | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (playerIndex + 1))
        , players =
            updatePlayerInPlayera playerIndex
                (\player2 ->
                    { player2
                        | route = Array.append player2.route player2.cardsToRoute
                        , selectedHandCardIndex = Nothing
                        , cardsToRoute = Array.empty
                        , cardToSharedPile = Nothing
                    }
                )
                gameInfo.players
        , sharedPile =
            Array.append gameInfo.sharedPile
                (Maybe.Extra.unwrap Array.empty
                    (\player2 -> Maybe.Extra.unwrap Array.empty (\card -> Array.fromList [ card ]) player2.cardToSharedPile)
                    player
                )
    }


startRoundEnd : GameInfo -> GameInfo
startRoundEnd gameInfo =
    { gameInfo | roundState = RevealSharedCard }


endRound : GameInfo -> GameInfo
endRound gameInfo =
    let
        isStageEnd =
            gameInfo.sharedPileSum >= 50 * (1 + Array.length gameInfo.players)
    in
    if isStageEnd then
        { gameInfo | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (gameInfo.roundNumber + 1)), roundNumber = gameInfo.roundNumber + 1 }
            |> fillPlayersHand

    else
        { gameInfo | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (gameInfo.roundNumber + 1)), roundNumber = gameInfo.roundNumber + 1 }
            |> fillPlayersHand


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
        , ( 50, Nachziehkarte 1 )
        , ( 50, Nachziehkarte 2 )
        , ( 50, Abwerfkarte )
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
            Array.fromList <| List.map (\name -> Player name 0 Array.empty Array.empty Nothing Array.empty Nothing) startInfo.players
    in
    GameInfo players 0 0 (NextPlayerInTurn 0) Array.empty Nothing 0 newSeed
        |> fillPlayersHand


fillPlayersHand : GameInfo -> GameInfo
fillPlayersHand gameInfo =
    let
        numCards =
            10 - Maybe.Extra.unwrap 0 (\player -> Array.length player.hand) (Array.get 0 gameInfo.players)

        createDrawDeck =
            Random.list (numCards * Array.length gameInfo.players) cardGenerator

        ( drawDeck, newSeed ) =
            Random.step createDrawDeck gameInfo.randomnessSeed

        takeCards index deck =
            Array.fromList <| List.take numCards <| List.drop (numCards * index) deck

        newPlayers =
            Array.indexedMap (\index player -> { player | hand = Array.append player.hand (takeCards index drawDeck) }) gameInfo.players
    in
    { gameInfo | players = newPlayers, randomnessSeed = newSeed }
