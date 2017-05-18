module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, type_, value, size, disabled)
import Html.Events exposing (onInput, onSubmit, onClick)


type alias Model =
    { players : List Player
    , playerName : String
    }


type alias Player =
    { name : String
    , scores : Scores
    }



{-
   Int = Score
   Bool = Has this been crossed off
-}


type alias Scores =
    Dict.Dict String ( Int, Bool )


initModel : Model
initModel =
    { players = []
    , playerName = ""
    }


initScores : Dict.Dict String ( Int, Bool )
initScores =
    Dict.empty


type Msg
    = Input Player String String
    | NameInput String
    | AddPlayer
    | Cross String Player


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title is-1" ] [ text "Elmy (Yahtzee)" ]
        , table [ class "table" ]
            [ tableHead model
            , tableBody model
            ]
        , p [] [ text (toString model) ]
        ]


tableHead : Model -> Html Msg
tableHead model =
    thead []
        [ tr []
            (th [] []
                :: (List.map
                        (\p -> th [] [ text p.name ])
                        model.players
                   )
                ++ [ th []
                        [ newPlayerForm model.playerName
                        ]
                   ]
            )
        ]


newPlayerForm : String -> Html Msg
newPlayerForm playerName =
    form [ onSubmit AddPlayer ]
        [ div [ class "field has-addons" ]
            [ p [ class "control" ]
                [ input
                    [ type_ "text"
                    , class "input is-small"
                    , value playerName
                    , onInput NameInput
                    ]
                    []
                ]
            , p [ class "control" ]
                [ input
                    [ type_ "submit"
                    , value "Lägg till"
                    , class "button is-small is-info"
                    ]
                    []
                ]
            ]
        ]


tableBody : Model -> Html Msg
tableBody model =
    tbody []
        [ tableRow model "Ett" "aces"
        , tableRow model "Två" "twos"
        , tableRow model "Tre" "threes"
        , tableRow model "Fyra" "fours"
        , tableRow model "Fem" "fives"
        , tableRow model "Sex" "sixes"
        , tableRowBonus model
        , tableRow model "Par" "pair"
        , tableRow model "Två par" "twoPairs"
        , tableRow model "Tretal" "threeOfAKind"
        , tableRow model "Fyrtal" "fourOfAKind"
        , tableRow model "Liten Stege" "smallStraight"
        , tableRow model "Stor Stege" "largeStraight"
        , tableRow model "Chans" "chance"
        , tableRow model "Yatzy" "yahtzee"
        , tableRowTotal model
        ]


tableRow : Model -> String -> String -> Html Msg
tableRow model label key =
    tr []
        (td []
            [ text label ]
            :: (model.players
                    |> List.map (tableCell key)
               )
        )


tableRowBonus : Model -> Html Msg
tableRowBonus model =
    tr []
        (td []
            [ text "Bonus" ]
            :: (model.players
                    |> List.map
                        (\p ->
                            td []
                                [ text (toString (playerBonus p))
                                ]
                        )
               )
        )


tableRowTotal : Model -> Html Msg
tableRowTotal model =
    tr []
        (td []
            [ text "Totalt" ]
            :: (model.players
                    |> List.map
                        (\p ->
                            td []
                                [ text (toString (playerTotal p))
                                ]
                        )
               )
        )


playerBonus : Player -> Int
playerBonus player =
    let
        singles =
            [ "aces", "twos", "threes", "fours", "fives", "sixes" ]

        total =
            player.scores
                |> Dict.filter (\k v -> List.member k singles)
                |> Dict.values
                |> List.map Tuple.first
                |> List.sum
    in
        if total > 62 then
            50
        else
            0


playerTotal : Player -> Int
playerTotal player =
    let
        total =
            player.scores
                |> Dict.values
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> List.sum
    in
        total + playerBonus player


tableCell : String -> Player -> Html Msg
tableCell key player =
    let
        val =
            case (Dict.get key player.scores) of
                Nothing ->
                    ""

                Just v ->
                    toString (Tuple.first v)

        dis =
            case (Dict.get key player.scores) of
                Nothing ->
                    False

                Just v ->
                    not (Tuple.second v)
    in
        td []
            [ div [ class "field has-addons" ]
                [ p [ class "control" ]
                    [ input
                        [ type_ "number"
                        , class "input is-small"
                        , onInput (Input player key)
                        , value val
                        , disabled dis
                        ]
                        []
                    ]
                , a
                    [ class "button is-small control bold"
                    , onClick (Cross key player)
                    ]
                    [ text "×"
                    ]
                ]
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input player key val ->
            let
                newVal =
                    case String.toInt val of
                        Err msg ->
                            Debug.log
                                msg
                                0

                        Ok v ->
                            v

                newPlayers =
                    List.map
                        (\p ->
                            if p.name == player.name then
                                { p | scores = Dict.insert key ( newVal, True ) p.scores }
                            else
                                p
                        )
                        model.players
            in
                { model | players = newPlayers }

        NameInput name ->
            { model | playerName = name }

        AddPlayer ->
            let
                newPlayers =
                    model.players ++ [ Player model.playerName initScores ]
            in
                { model
                    | players = newPlayers
                    , playerName = ""
                }

        Cross key player ->
            let
                newPlayers =
                    List.map
                        (\p ->
                            if p.name == player.name then
                                let
                                    newScores =
                                        Dict.insert key ( 0, False ) p.scores
                                in
                                    { p | scores = newScores }
                            else
                                p
                        )
                        model.players
            in
                { model | players = newPlayers }
