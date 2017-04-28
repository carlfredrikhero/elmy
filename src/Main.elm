module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, type_, value, size)
import Html.Events exposing (onInput)


type alias Model =
    { players : List Player
    }


type alias Player =
    { name : String
    , scores : Scores
    }


type alias Scores =
    Dict.Dict String Int


initModel : Model
initModel =
    { players =
        [ Player "Carl-Fredrik" initScores
        , Player "Philip" initScores
        , Player "Hugo" initScores
        , Player "Theres" initScores
        ]
    }


initScores : Dict.Dict String Int
initScores =
    Dict.empty


type Msg
    = Input Player String String


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
                ++ [ th [] [ text "Add" ] ]
            )
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
        total =
            player.scores
                |> Dict.values
                |> List.take 6
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
                    toString v
    in
        td []
            [ input
                [ type_ "number"
                , class "input is-small"
                , onInput (Input player key)
                ]
                []
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input player key val ->
            -- 1. Find player
            -- 2. Update score key
            let
                newVal =
                    case String.toInt val of
                        Err msg ->
                            0

                        Ok v ->
                            v

                newPlayers =
                    List.map
                        (\p ->
                            if p.name == player.name then
                                { p | scores = Dict.insert key newVal p.scores }
                            else
                                p
                        )
                        model.players
            in
                { model | players = newPlayers }
