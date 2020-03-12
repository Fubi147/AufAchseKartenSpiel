module GameView exposing (..)

import Array exposing (Array)
import GameLogic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (disabled, src, style, width)
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


viewCardImage : Card -> Bool -> Bool -> List (Attribute Msg) -> Html Msg
viewCardImage card highlighted showBack onClickHandler =
    let
        highlightedAttributes =
            if highlighted then
                [ style "margin-bottom" "8px" ]

            else
                []

        imageFileName =
            if showBack then
                "-back"

            else
                case card of
                    Speed speed ->
                        "" ++ String.fromInt speed

                    Minus50 ->
                        "-50"

                    ServiceStation ->
                        "-service"

                    DrawCard numberCardsToDraw ->
                        "+" ++ String.fromInt numberCardsToDraw

                    Discard ->
                        "-discard"
    in
    img ([ src ("svg/card" ++ imageFileName ++ ".png"), width 120 ] ++ highlightedAttributes ++ onClickHandler) []


viewPlayer : GameInfo -> Int -> Player -> Html Msg
viewPlayer gameInfo playerIndex player =
    let
        isPlayerInTurn =
            gameInfo.roundState == PlayerInTurn playerIndex || gameInfo.roundState == RevealSharedPileCardPlayerInTurn playerIndex

        maxNumberCardsToRoute =
            if gameInfo.roundState == PlayerInTurn playerIndex then
                1

            else if gameInfo.roundState == RevealSharedPileCardPlayerInTurn playerIndex then
                case gameInfo.sharedPileCard of
                    Just (DrawCard numberCardsToDraw) ->
                        numberCardsToDraw

                    _ ->
                        0

            else
                0

        handCards =
            div []
                (Array.toList
                    (Array.indexedMap
                        (\cardIndex card ->
                            if isPlayerInTurn then
                                viewCardImage card (Just cardIndex == player.selectedHandCardIndex) False [ onClick (HandCardClicked playerIndex cardIndex) ]

                            else
                                viewCardImage card False True []
                        )
                        player.hand
                    )
                )

        lastToRouteCard =
            Array.get (Array.length player.cardsToRoute - 1) player.cardsToRoute

        routeCardsHidden =
            gameInfo.roundState /= StageEnd || not isPlayerInTurn

        routeCards =
            div []
                ([ text "Route: " ]
                    ++ (Array.toList (Array.map (\card -> viewCardImage card False routeCardsHidden []) player.route)
                            ++ Array.toList (Array.map (\card -> viewCardImage card False False []) player.cardsToRoute)
                            ++ [ if Array.length player.cardsToRoute < maxNumberCardsToRoute then
                                    Maybe.Extra.unwrap (text "") (\index -> button [ onClick <| AddToRouteClicked playerIndex index ] [ text "Add To Route" ]) player.selectedHandCardIndex

                                 else
                                    text ""
                               ]
                            ++ [ if isPlayerInTurn then
                                    Maybe.Extra.unwrap (text "") (\card -> button [ onClick <| TakeRouteCardBackClicked playerIndex card ] [ text "Take Card Back" ]) lastToRouteCard

                                 else
                                    text ""
                               ]
                       )
                )
    in
    div []
        [ text (player.name ++ ": ")
        , handCards
        , routeCards
        ]


viewSharedPile : GameInfo -> Html Msg
viewSharedPile gameInfo =
    let
        sharedPile =
            Array.toList (Array.map (\card -> viewCardImage card False True []) gameInfo.sharedPile)

        sharedPileCard =
            Maybe.Extra.unwrap [] (\card -> [ viewCardImage card False False [] ]) gameInfo.sharedPileCard

        playerInTurnToPlaceSharedPileCard =
            case gameInfo.roundState of
                PlayerInTurn playerIndex ->
                    Just playerIndex

                _ ->
                    Nothing
    in
    div []
        ([ text "Shared Pile:"
         , div [] [ text <| "Sum: " ++ String.fromInt gameInfo.sharedPileSum ]
         ]
            ++ sharedPile
            ++ sharedPileCard
            ++ (case playerInTurnToPlaceSharedPileCard of
                    Just playerIndex ->
                        case gameInfo.sharedPileCard of
                            Just card ->
                                [ button [ onClick <| TakeSharedPileCardBackClicked playerIndex card ] [ text "Take Card Back" ] ]

                            Nothing ->
                                if getSelectedCardFromPlayersHand playerIndex gameInfo.players == Nothing then
                                    []

                                else
                                    [ button [ onClick <| AddToSharedPileClicked playerIndex ] [ text "Add To Pile" ] ]

                    Nothing ->
                        []
               )
        )


viewRoundStateButton : GameInfo -> Html Msg
viewRoundStateButton gameInfo =
    let
        startButton playerIndex =
            button [ onClick <| StartTurnClicked ] [ text <| "Start " ++ Maybe.Extra.unwrap "" (\player -> player.name) (Array.get playerIndex gameInfo.players) ++ "'s Turn" ]
    in
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            startButton playerIndex

        PlayerInTurn playerIndex ->
            let
                isButtonDisabled =
                    Maybe.Extra.unwrap True (\player -> Array.isEmpty player.cardsToRoute || Maybe.Extra.isNothing gameInfo.sharedPileCard) (Array.get playerIndex gameInfo.players)
            in
            button [ disabled isButtonDisabled, onClick <| EndTurnClicked ] [ text "End Turn" ]

        RevealSharedPileCard ->
            let
                nextSharedPileCard =
                    Array.get (Array.length gameInfo.sharedPile - 1) gameInfo.sharedPile
            in
            if nextSharedPileCard == Nothing then
                button [ onClick <| RevealSharedPileCardClicked ]
                    [ if isStageEnd gameInfo then
                        text "End Stage"

                      else
                        text "Next Round"
                    ]

            else
                button [ onClick <| RevealSharedPileCardClicked ] [ text "Reveal Shared Pile Card" ]

        RevealSharedPileCardNextPlayerInTurn playerIndex ->
            startButton playerIndex

        RevealSharedPileCardPlayerInTurn playerIndex ->
            case gameInfo.sharedPileCard of
                Just (DrawCard numberCardsToDraw) ->
                    let
                        isButtonDisabled =
                            Maybe.Extra.unwrap True (\player -> Array.length player.cardsToRoute /= numberCardsToDraw) (Array.get playerIndex gameInfo.players)
                    in
                    button [ disabled isButtonDisabled, onClick <| EndTurnClicked ] [ text "End Turn" ]

                _ ->
                    button [ onClick <| RevealSharedPileCardClicked ] [ text "Reveal Shared Pile Card" ]

        StageEnd ->
            if gameInfo.stageNumber >= 4 then
                button [ onClick EndGameClicked ] [ text "End Game" ]

            else
                button [ onClick NextStageClicked ] [ text "Next Stage" ]


viewGameStats : GameInfo -> Html Msg
viewGameStats gameInfo =
    div []
        [ text "Score: "
        , gameInfo.players
            |> Array.map (\player -> div [] [ player.name ++ ": " ++ String.fromInt player.score |> text ])
            |> Array.toList
            |> div []
        ]


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    div []
        [ Array.indexedMap (viewPlayer gameInfo) gameInfo.players
            |> Array.toList
            |> List.intersperse (div [] [ br [] [] ])
            |> div []
        , div [] [ br [] [] ]
        , viewSharedPile gameInfo
        , div [] [ br [] [] ]
        , viewRoundStateButton gameInfo
        , div [] [ br [] [] ]
        , viewGameStats gameInfo
        ]
