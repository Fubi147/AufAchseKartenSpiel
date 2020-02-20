module Model exposing (..)

import Array exposing (Array)
import Array.Extra
import Maybe.Extra
import Random exposing (Seed)


type alias Model =
    { gameState : GameState
    }


type GameState
    = Start StartInfo
    | Play GameInfo
    | End


type alias StartInfo =
    { players : List String
    }


type alias GameInfo =
    { players : Array Player
    , stageNumber : Int
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
    | RevealSharedCard
    | ShowSharedCard Card


type alias Player =
    { name : String
    , points : Int
    , hand : Array Card
    , route : Array Card
    }


type Card
    = Speed Int
    | Minus50
    | Tankstelle
    | Nachziehkarte Int
    | Abwerfkarte


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
    | RevealSharedDeckCardClicked


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
