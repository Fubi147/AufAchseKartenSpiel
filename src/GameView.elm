module GameView exposing (..)

import Array exposing (Array)
import Bool.Extra
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Maybe.Extra
import Model exposing (..)


viewCards mf cards =
    Array.toList <| Array.indexedMap mf cards


showCardFront playerIndex player =
    \cardIndex card ->
        let
            colorAttributes =
                Maybe.Extra.unwrap [] (\index -> Bool.Extra.ifElse [ style "background-color" "#73D216" ] [] (index == cardIndex)) player.selectedHandCardIndex
        in
        button ([ onClick (HandCardClicked playerIndex cardIndex) ] ++ colorAttributes) [ text <| Debug.toString card ]


showCardBack attributes =
    \_ _ -> button attributes [ text "X" ]


viewPlayer : GameInfo -> Int -> Player -> Html Msg
viewPlayer gameInfo playerIndex player =
    let
        playerInTurn =
            case gameInfo.roundState of
                NextPlayerInTurn _ ->
                    False

                PlayerInTurn int ->
                    int == playerIndex

                RevealSharedCard ->
                    False

                RevealSharedCardPlayerInTurn card int ->
                    False

        playerRouteCards =
            Array.toList <|
                Array.map
                    (\card ->
                        button [ disabled True ]
                            [ text <|
                                if playerInTurn then
                                    Debug.toString card

                                else
                                    "X"
                            ]
                    )
                    player.cardsToRoute
    in
    div []
        [ text <| player.name ++ ": "
        , div [] <|
            viewCards
                (if playerInTurn then
                    showCardFront playerIndex player

                 else
                    showCardBack [ disabled True ]
                )
                player.hand
        , div []
            (viewCards (showCardBack [ disabled True ]) player.route
                ++ playerRouteCards
                ++ [ let
                        lastToRouteCard =
                            Array.get (Array.length player.cardsToRoute - 1) player.cardsToRoute
                     in
                     if playerInTurn then
                        Maybe.Extra.unwrap
                            (Maybe.Extra.unwrap (text "") (\index -> button [ onClick <| AddToRouteClicked playerIndex index ] [ text "Add To Route" ]) player.selectedHandCardIndex)
                            (\card -> button [ onClick <| TakeRouteCardBackClicked playerIndex card ] [ text "Take Card Back" ])
                            lastToRouteCard

                     else
                        text ""
                   ]
            )
        ]


viewSharedPile : GameInfo -> Html Msg
viewSharedPile gameInfo =
    div []
        [ text "Shared Pile:"
        , div []
            (viewCards (showCardBack [ disabled True ]) gameInfo.sharedPile
                ++ (Array.toList <|
                        Array.indexedMap
                            (\playerIndex player ->
                                Maybe.Extra.unwrap
                                    (Maybe.Extra.unwrap (text "")
                                        (\index -> button [ onClick <| AddToSharedPileClicked playerIndex index ] [ text "Add To Pile" ])
                                        player.selectedHandCardIndex
                                    )
                                    (\card ->
                                        let
                                            playerInTurn =
                                                case gameInfo.roundState of
                                                    NextPlayerInTurn _ ->
                                                        False

                                                    PlayerInTurn int ->
                                                        int == playerIndex

                                                    RevealSharedCard ->
                                                        False

                                                    RevealSharedCardPlayerInTurn _ _ ->
                                                        False
                                        in
                                        if playerInTurn then
                                            span []
                                                [ button [ disabled True ] [ text <| Debug.toString card ]
                                                , button [ onClick <| TakeSharedPileCardBackClicked playerIndex card ] [ text "Take Card Back" ]
                                                ]

                                        else
                                            button [ disabled True ] [ text "X" ]
                                    )
                                    player.cardToSharedPile
                            )
                            gameInfo.players
                   )
            )
        ]


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    div []
        [ div [] <| Array.toList <| Array.indexedMap (viewPlayer gameInfo) gameInfo.players
        , viewSharedPile gameInfo
        , div [] [ br [] [] ]
        , case gameInfo.roundState of
            NextPlayerInTurn playerIndex ->
                button [ onClick <| StartTurnClicked playerIndex ] [ text <| "Start " ++ Maybe.Extra.unwrap "" (\player -> player.name) (Array.get playerIndex gameInfo.players) ++ "'s Turn" ]

            PlayerInTurn playerIndex ->
                let
                    isButtonDisabled =
                        Maybe.Extra.unwrap True (\player -> Array.isEmpty player.cardsToRoute || Maybe.Extra.isNothing player.cardToSharedPile) (Array.get playerIndex gameInfo.players)
                in
                button [ disabled isButtonDisabled, onClick <| EndTurnClicked playerIndex ] [ text "End Turn" ]

            RevealSharedCard ->
                button [ onClick <| RevealSharedPileCardClicked ] [ text "Reveal Shared Card" ]

            RevealSharedCardPlayerInTurn sharedCard playerIndex ->
                let
                    isButtonDisabled =
                        Maybe.Extra.unwrap True (\player -> Array.isEmpty player.cardsToRoute || Maybe.Extra.isNothing player.cardToSharedPile) (Array.get playerIndex gameInfo.players)
                in
                button [ disabled isButtonDisabled, onClick <| EndTurnClicked playerIndex ] [ text "End Turn" ]
        ]
