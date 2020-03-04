module Model exposing (..)

import Array exposing (Array)
import Array.Extra
import Bool.Extra
import Maybe.Extra
import Random exposing (Seed)


type alias Model =
    { gameState : GameState }


type GameState
    = Start StartInfo
    | Play GameInfo
    | End


type alias StartInfo =
    { players : List String }


type alias GameInfo =
    { players : Array Player
    , stageNumber : Int
    , roundNumber : Int
    , roundState : RoundState
    , sharedPile : Array Card
    , sharedPileCard : Maybe Card
    , sharedPileSum : Int
    , randomnessSeed : Seed
    }


type RoundState
    = NextPlayerInTurn Int
    | PlayerInTurn Int
    | RevealSharedPileCard
    | RevealSharedPileCardNextPlayerInTurn Int
    | RevealSharedPileCardPlayerInTurn Int
    | StageEnd


type alias Player =
    { name : String
    , score : Int
    , hand : Array Card
    , route : Array Card
    , selectedHandCardIndex : Maybe Int
    , cardsToRoute : Array Card
    }


type Card
    = Speed Int
    | Minus50
    | ServiceStation
    | DrawCard Int
    | Discard


type Msg
    = NameChanged Int String
    | RemovePlayer Int
    | AddPlayer
    | StartGame StartInfo
    | GameStarted StartInfo Seed
    | StartTurnClicked
    | HandCardClicked Int Int
    | AddToRouteClicked Int Int
    | TakeRouteCardBackClicked Int Card
    | AddToSharedPileClicked Int
    | TakeSharedPileCardBackClicked Int Card
    | EndTurnClicked
    | RevealSharedPileCardClicked
    | NextStageClicked


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


updateGameStageState : (RoundState -> RoundState) -> Model -> Model
updateGameStageState updateFunction model =
    case model.gameState of
        Play gameInfo ->
            { model | gameState = Play { gameInfo | roundState = updateFunction gameInfo.roundState } }

        _ ->
            model


updatePlayer : Int -> (Player -> Player) -> Model -> Model
updatePlayer playerIndex updateFunction model =
    updateGameInfo (\gameInfo -> { gameInfo | players = Array.indexedMap (\index player -> Bool.Extra.ifElse (updateFunction player) player (index == playerIndex)) gameInfo.players }) model


updatePlayerInPlayers : Int -> (Player -> Player) -> Array Player -> Array Player
updatePlayerInPlayers playerIndex updateFunction players =
    Array.indexedMap (\index player -> Bool.Extra.ifElse (updateFunction player) player (index == playerIndex)) players


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


addCardToPlayersRoute : Int -> Maybe Card -> Array Player -> Array Player
addCardToPlayersRoute playerIndex card players =
    case card of
        Just card2 ->
            Array.indexedMap
                (\playerIndex2 player ->
                    if playerIndex == playerIndex2 then
                        { player | route = Array.push card2 player.route }

                    else
                        player
                )
                players

        Nothing ->
            players


getSelectedCardFromPlayersHand : Int -> Array Player -> Maybe Card
getSelectedCardFromPlayersHand playerIndex players =
    let
        currentPlayer =
            Array.get playerIndex players
    in
    Maybe.Extra.join <| Maybe.map (\player -> Maybe.Extra.unwrap Nothing (\selectedHandCardIndex -> Array.get selectedHandCardIndex player.hand) player.selectedHandCardIndex) currentPlayer


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
