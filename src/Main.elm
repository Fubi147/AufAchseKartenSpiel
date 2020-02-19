module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { gameState : GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model <| Start <| StartInfo [ "Player 1" ]
    , Cmd.none
    )


type GameState
    = Start StartInfo
    | Play GameInfo
    | End


type alias StartInfo =
    { players : List String
    }


type alias GameInfo =
    { players : List Player
    , round : Int
    , playerInTurn : Int
    , middleDeck : List Card
    , middleDeckSum : Int
    , randomnessSeed : Seed
    }


type alias Player =
    { name : String
    , hand : List Card
    , route : List Card
    }


type Card
    = Speed Int
    | Minus50
    | Tankstelle
    | Nachziehkarte Int
    | Abwerfkarte



-- UPDATE


type Msg
    = NameChanged Int String
    | RemovePlayer Int
    | AddPlayer
    | StartGame
    | GameStarted Seed
    | HandCardClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( updateStartInfo (\startInfo -> StartInfo (startInfo.players ++ [ "Player " ++ (String.fromInt <| 1 + List.length startInfo.players) ])) model, Cmd.none )

        NameChanged index newName ->
            ( updateStartInfo
                (\startInfo ->
                    StartInfo
                        (List.indexedMap
                            (\index_ name ->
                                if index_ == index then
                                    newName

                                else
                                    name
                            )
                            startInfo.players
                        )
                )
                model
            , Cmd.none
            )

        RemovePlayer index ->
            ( updateStartInfo (\startInfo -> StartInfo (List.take index startInfo.players ++ List.drop (index + 1) startInfo.players)) model, Cmd.none )

        StartGame ->
            ( model, Random.generate GameStarted Random.independentSeed )

        GameStarted seed ->
            ( Model <| Play <| initGame model seed, Cmd.none )

        HandCardClicked int ->
            ( updateGameInfo (\gameInfo -> gameInfo) model, Cmd.none )


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform (Speed 0)
        [ Minus50
        , Tankstelle
        , Nachziehkarte 0
        , Abwerfkarte
        ]


initGame : Model -> Seed -> GameInfo
initGame model seed =
    let
        numCards =
            10

        startInfo =
            case model.gameState of
                Start startInfo2 ->
                    startInfo2

                _ ->
                    StartInfo []

        createDrawDeck =
            Random.list (numCards * List.length startInfo.players) cardGenerator

        ( drawDeck, newSeed ) =
            Random.step createDrawDeck seed

        takeCards index deck =
            List.take 10 <| List.drop (10 * index) deck

        players =
            List.indexedMap (\index name -> Player name (takeCards index drawDeck) []) startInfo.players
    in
    GameInfo players 0 0 [] 0 newSeed


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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


viewStartInfo : StartInfo -> Html Msg
viewStartInfo startInfo =
    let
        mapPlayersToInput =
            \index name ->
                div []
                    [ text (String.fromInt (index + 1) ++ ". Player Name: ")
                    , input [ value name, onInput <| NameChanged index ] []
                    , button [ onClick (RemovePlayer index) ] [ text "Remove Player" ]
                    ]
    in
    div []
        [ div [] <| List.indexedMap mapPlayersToInput startInfo.players
        , button [ onClick AddPlayer ] [ text "Add Player" ]
        , button [ onClick StartGame ] [ text "Start Game" ]
        ]


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    let
        viewCards mf cards =
            div [] <| List.indexedMap mf cards

        viewPlayer index player =
            let
                showText =
                    \_ card -> text <| Debug.toString card

                showButton =
                    \cardIndex card -> button [ onClick (HandCardClicked cardIndex) ] [ text <| Debug.toString card ]
            in
            div []
                [ text <| player.name ++ ": "
                , div []
                    [ viewCards showButton player.hand
                    , br [] []
                    , viewCards showText player.route
                    , br [] []
                    ]
                ]

        viewStats =
            div [] [ text <| "Sum: " ++ String.fromInt gameInfo.middleDeckSum ]
    in
    div []
        [ div [] <| List.indexedMap viewPlayer gameInfo.players
        , viewStats
        ]
