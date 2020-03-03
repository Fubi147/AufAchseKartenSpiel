module GameView exposing (..)

import Array exposing (Array)
import GameLogic exposing (isLastPlayerInRound)
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Maybe.Extra
import Model exposing (..)


viewCard : Card -> Bool -> Bool -> List (Attribute Msg) -> Html Msg
viewCard card highlighted showBack onClickHandler =
    let
        colorAttributes =
            case highlighted of
                True ->
                    [ style "background-color" "#73D216" ]

                False ->
                    []

        cardText =
            case showBack of
                True ->
                    "X"

                False ->
                    Debug.toString card

        isDisabled =
            case List.isEmpty onClickHandler of
                True ->
                    [ disabled True ]

                False ->
                    []
    in
    button (onClickHandler ++ isDisabled ++ colorAttributes) [ text cardText ]


viewPlayer2 : GameInfo -> Int -> Player -> Html Msg
viewPlayer2 gameInfo playerIndex player =
    div []
        [ text <| player.name ++ ": " ]


viewCards mf cards =
    Array.toList <| Array.indexedMap mf cards


showCardFront playerIndex player =
    \cardIndex card ->
        viewCard
            card
            (Maybe.Extra.unwrap False (\index -> index == cardIndex) player.selectedHandCardIndex)
            False
            [ onClick (HandCardClicked playerIndex cardIndex) ]


showCardBack : List (Attribute Msg) -> (Int -> Card -> Html Msg)
showCardBack attributes =
    \_ _ -> button attributes [ text "X" ]


viewPlayer : GameInfo -> Int -> Player -> Html Msg
viewPlayer gameInfo playerIndex player =
    let
        playerInTurn =
            case gameInfo.roundState of
                PlayerInTurn playerIndex2 ->
                    playerIndex == playerIndex2

                RevealSharedCardPlayerInTurn sharedCard playerIndex2 turnStarted ->
                    playerIndex == playerIndex2 && turnStarted

                _ ->
                    False

        showAddToRoute =
            case gameInfo.roundState of
                RevealSharedCardPlayerInTurn (Nachziehkarte numCards) playerIndex2 turnStarted ->
                    playerInTurn && Array.length player.cardsToRoute < numCards

                _ ->
                    playerInTurn && Array.length player.cardsToRoute < 1

        playerRouteCards =
            let
                isDiscard =
                    case gameInfo.roundState of
                        RevealSharedCardPlayerInTurn Abwerfkarte _ _ ->
                            True

                        _ ->
                            False
            in
            Array.toList <|
                Array.map
                    (\card ->
                        button [ disabled True ]
                            [ text <|
                                if playerInTurn || isDiscard then
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
            (viewCards
                (case gameInfo.roundState of
                    StageEnd ->
                        \index card -> button [ disabled True ] [ text <| Debug.toString card ]

                    _ ->
                        showCardBack [ disabled True ]
                )
                player.route
                ++ playerRouteCards
                ++ [ if showAddToRoute then
                        Maybe.Extra.unwrap (text "") (\index -> button [ onClick <| AddToRouteClicked playerIndex index ] [ text "Add To Route" ]) player.selectedHandCardIndex

                     else
                        text ""
                   ]
                ++ [ let
                        lastToRouteCard =
                            Array.get (Array.length player.cardsToRoute - 1) player.cardsToRoute
                     in
                     if playerInTurn then
                        Maybe.Extra.unwrap (text "") (\card -> button [ onClick <| TakeRouteCardBackClicked playerIndex card ] [ text "Take Card Back" ]) lastToRouteCard

                     else
                        text ""
                   ]
            )
        ]


viewSharedPile : GameInfo -> Html Msg
viewSharedPile gameInfo =
    div []
        [ text "Shared Pile:"
        , div [] [ text <| "Sum: " ++ String.fromInt gameInfo.sharedPileSum ]
        , div []
            (viewCards (showCardBack [ disabled True ]) gameInfo.sharedPile
                ++ (Array.toList <|
                        Array.indexedMap
                            (\playerIndex player ->
                                let
                                    playerInTurn =
                                        case gameInfo.roundState of
                                            PlayerInTurn int ->
                                                int == playerIndex

                                            _ ->
                                                False
                                in
                                Maybe.Extra.unwrap
                                    (Maybe.Extra.unwrap (text "")
                                        (\index ->
                                            if playerInTurn then
                                                button [ onClick <| AddToSharedPileClicked playerIndex index ] [ text "Add To Pile" ]

                                            else
                                                text ""
                                        )
                                        player.selectedHandCardIndex
                                    )
                                    (\card ->
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
                ++ (case gameInfo.roundState of
                        RevealSharedCardPlayerInTurn sharedCard playerIndex turnStarted ->
                            [ button [ disabled True ] [ text <| Debug.toString sharedCard ] ]

                        _ ->
                            []
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

            RevealSharedCardPlayerInTurn sharedCard playerIndex turnStarted ->
                let
                    endButton notEnabled =
                        let
                            buttonText =
                                if Array.isEmpty gameInfo.sharedPile then
                                    "End Round"

                                else
                                    "Reveal Next Shared Card"

                            isButtonDisabled =
                                case sharedCard of
                                    Nachziehkarte int ->
                                        True

                                    _ ->
                                        False
                        in
                        button [ disabled (notEnabled && isButtonDisabled), onClick <| RevealSharedPileCardClicked ] [ text buttonText ]
                in
                case sharedCard of
                    Nachziehkarte numCards ->
                        if turnStarted then
                            let
                                isButtonDisabled =
                                    Maybe.Extra.unwrap True
                                        (\player ->
                                            (Array.length player.cardsToRoute /= numCards)
                                                && not (Array.isEmpty player.hand)
                                        )
                                        (Array.get playerIndex gameInfo.players)
                            in
                            button [ disabled isButtonDisabled, onClick <| EndTurnClicked playerIndex ] [ text "End Turn" ]

                        else if isLastPlayerInRound gameInfo then
                            endButton False

                        else
                            button [ onClick <| StartTurnClicked playerIndex ] [ text <| "Start " ++ Maybe.Extra.unwrap "" (\player -> player.name) (Array.get playerIndex gameInfo.players) ++ "'s Turn" ]

                    _ ->
                        endButton True

            StageEnd ->
                button [ onClick NextStageClicked ] [ text "Next Stage" ]
        ]
