module View exposing (..)

import Array exposing (Array)
import GameLogic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Model exposing (..)


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    let
        viewCards mf cards =
            div [] <| Array.toList <| Array.indexedMap mf cards

        showCardBack attributes =
            \_ _ -> button attributes [ text "X" ]

        viewPlayer playerIndex player =
            let
                showCardFront =
                    \cardIndex card ->
                        let
                            colorAttributes =
                                case gameInfo.cardAction of
                                    HandCardSelected selectedCardIndex ->
                                        if selectedCardIndex == cardIndex then
                                            [ style "background-color" "#73D216" ]

                                        else
                                            []

                                    _ ->
                                        []
                        in
                        button
                            ([ onClick (HandCardClicked cardIndex) ] ++ colorAttributes)
                            [ text <| Debug.toString card ]

                playerRouteCard =
                    gameInfo.cardToRoute
                        |> Maybe.map
                            (\card ->
                                div []
                                    [ button [ disabled True ] [ text <| Debug.toString card ]
                                    , button [ onClick TakeRouteCardBackClicked ] [ text "Take Card Back" ]
                                    ]
                            )
                        |> Maybe.withDefault (text "")
            in
            div []
                [ text <| player.name ++ ": "
                , div []
                    [ viewCards
                        (if playerIndex == gameInfo.playerInTurn then
                            showCardFront

                         else
                            showCardBack [ disabled True ]
                        )
                        player.hand
                    , viewCards (showCardBack [ disabled True ]) player.route
                    , if playerIndex == gameInfo.playerInTurn then
                        div []
                            [ playerRouteCard
                            , case gameInfo.cardAction of
                                HandCardSelected selectedHandCardIndex ->
                                    case gameInfo.cardToRoute of
                                        Nothing ->
                                            button [ onClick <| AddToRouteClicked selectedHandCardIndex ] [ text "Add To Route" ]

                                        _ ->
                                            text ""

                                _ ->
                                    text ""
                            ]

                      else
                        text ""
                    , div [] [ br [] [] ]
                    ]
                ]

        viewShared =
            let
                playerSharedCard =
                    gameInfo.cardToSharedDeck
                        |> Maybe.map
                            (\card ->
                                div []
                                    [ button [ disabled True ] [ text <| Debug.toString card ]
                                    , button [ onClick TakeSharedCardBackClicked ] [ text "Take Card Back" ]
                                    ]
                            )
                        |> Maybe.withDefault (text "")
            in
            div []
                [ text "Shared Deck:"
                , br [] []
                , text <| "Sum: " ++ String.fromInt (gameInfo.sharedDeckSum + Maybe.Extra.unwrap 0 getCardValue gameInfo.cardToSharedDeck)
                , div []
                    [ viewCards (showCardBack [ disabled True ]) gameInfo.sharedDeck
                    , playerSharedCard
                    , case gameInfo.cardAction of
                        HandCardSelected selectedHandCardIndex ->
                            case gameInfo.cardToSharedDeck of
                                Nothing ->
                                    button [ onClick <| AddToSharedDeckClicked selectedHandCardIndex ] [ text "Add To Shared Deck" ]

                                _ ->
                                    text ""

                        RevealSharedCard ->
                            button [ onClick <| RevealSharedDeckCardClicked ] [ text "Reveal Next Card" ]

                        ShowSharedCard card ->
                            button [ disabled True ] [ text <| Debug.toString card ]

                        NoAction ->
                            text ""
                    ]
                ]
    in
    div []
        [ div [] <| Array.toList <| Array.indexedMap viewPlayer gameInfo.players
        , viewShared
        , div [] [ br [] [] ]
        , button
            [ disabled (Maybe.Extra.isNothing gameInfo.cardToRoute || Maybe.Extra.isNothing gameInfo.cardToSharedDeck)
            , onClick EndTurn
            ]
            [ text "End Turn" ]
        ]
