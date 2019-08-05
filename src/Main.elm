module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random as R exposing (Generator)


greetings : Generator String
greetings =
    R.uniform
        "listen"
        [ "please"
        , "wait"
        , "hello"
        , "it's you"
        ]


subjects : Generator String
subjects =
    R.uniform
        "adventurer"
        [ "traveler"
        , "hero"
        , "my child"
        ]


type Action
    = Steal
    | Slay
    | Save
    | Find
    | Destroy
    | Win
    | Protect


actions : Generator Action
actions =
    R.uniform
        Steal
        [ Slay
        , Save
        , Find
        , Destroy
        , Win
        , Protect
        ]


actionToString : Action -> String
actionToString action =
    case action of
        Steal ->
            "steal"

        Slay ->
            "slay"

        Save ->
            "save"

        Find ->
            "find"

        Destroy ->
            "destroy"

        Win ->
            "win"

        Protect ->
            "protect"


type Object
    = Treasure
    | Dragon
    | Princess
    | Amulet
    | Portal
    | War
    | Castle


objects : Generator Object
objects =
    R.uniform
        Treasure
        [ Dragon
        , Princess
        , Amulet
        , Portal
        , War
        , Castle
        ]


objectToString : Object -> String
objectToString item =
    case item of
        Treasure ->
            "treasure"

        Dragon ->
            "dragon"

        Princess ->
            "princess"

        Amulet ->
            "amulet"

        Portal ->
            "portal"

        War ->
            "war"

        Castle ->
            "castle"


capitalize : String -> String
capitalize str =
    String.uncons str
        |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
        |> Maybe.withDefault str


intros : Scenario -> Generator (Html msg)
intros scenario =
    R.map (say scenario.npc.name)
        (quests scenario)


say : String -> String -> Html msg
say name line =
    p [] [ span [] [ strong [] [ text (name ++ ": ") ], text line ] ]


dialogues : Model -> Generator (Html Msg)
dialogues model =
    R.map2 (\intro choice -> div [] [ intro, choice ])
        (intros model.scenario)
        (choices model.scenario)


nonsenseResponses : Scenario -> Generator String
nonsenseResponses scenario =
    R.uniform
        "That doesn't make any sense."
        [ "What does that even mean?"
        , "Wait... '"
            ++ actionToString scenario.objective.action
            ++ "' the '"
            ++ objectToString scenario.objective.object
            ++ "'?"
        ]


grossedOutResponses : Scenario -> Generator String
grossedOutResponses scenario =
    R.uniform
        "Ew, no."
        [ "Gross! No way!"
        , "That's messed up for sure."
        ]


confusedResponses : Generator String
confusedResponses =
    R.uniform
        "What?"
        [ "Why me?"
        , "Who are you?"
        ]


isNonsense : Scenario -> Bool
isNonsense { objective } =
    List.member ( objective.action, objective.object )
        [ ( Slay, Portal )
        , ( Destroy, War )
        , ( Win, Princess )
        , ( Steal, Castle )
        , ( Protect, War )
        ]


isGrossAf : Scenario -> Bool
isGrossAf { objective } =
    List.member ( objective.action, objective.object )
        [ ( Destroy, Princess )
        ]


choices : Scenario -> Generator (Html Msg)
choices scenario =
    R.map3
        (\yes no wat ->
            case scenario.choice of
                Just choice ->
                    say "You"
                        (case choice of
                            Yes ->
                                yes

                            No ->
                                no

                            Wat ->
                                wat
                        )

                Nothing ->
                    p []
                        [ button
                            [ Attr.style "color" "green"
                            , Events.onClick (Choose Yes)
                            ]
                            [ text yes ]
                        , span [] [ text "  " ]
                        , button
                            [ Attr.style "color" "red"
                            , Events.onClick (Choose No)
                            ]
                            [ text no ]
                        , span [] [ text "  " ]
                        , button
                            [ Attr.style "color" "black"
                            , Events.onClick (Choose Wat)
                            ]
                            [ text wat ]
                        ]
        )
        affirmativeResponses
        negativeResponses
        (if isNonsense scenario then
            nonsenseResponses scenario

         else if isGrossAf scenario then
            grossedOutResponses scenario

         else
            confusedResponses
        )


names : Generator String
names =
    R.map2 (\desc noun -> [ desc, noun ] |> List.map capitalize |> String.join " ")
        adjectives
        folks


adjectives : Generator String
adjectives =
    R.uniform "hooded"
        [ "mysterious"
        , "young"
        , "old"
        , "tired"
        , "worried"
        , "delighted"
        , "hungry"
        , "ragged"
        , "ugly"
        , "beautiful"
        , "handsome"
        , "quirky"
        , "ravenous"
        ]


folks : Generator String
folks =
    R.uniform "figure"
        [ "woman"
        , "man"
        , "child"
        , "peasant"
        , "noble"
        , "butcher"
        , "blacksmith"
        ]


affirmativeResponses : Generator String
affirmativeResponses =
    R.uniform
        "How can I help?"
        [ "Okay!"
        , "That sounds important!"
        , "You got it."
        , "I'm on it!"
        ]


negativeResponses : Generator String
negativeResponses =
    R.uniform
        "Eh. Not really."
        [ "I'd rather not."
        , "Not today!"
        , "Don't wanna."
        , "Nah."
        , "Haha, no."
        ]


quests : Scenario -> Generator String
quests scenario =
    R.map2
        (\imploration subject ->
            String.join " "
                [ imploration ++ ","
                , subject ++ "!"
                , "You"
                , "must"
                , actionToString scenario.objective.action
                , "the"
                , objectToString scenario.objective.object
                ]
                ++ "!"
                |> capitalize
        )
        greetings
        subjects


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    let
        initialSeed =
            "lmao"
    in
    { seed = initialSeed
    , scenario = generateScenario initialSeed
    }


generateScenario : String -> Scenario
generateScenario =
    seedify >> R.step scenarios >> Tuple.first


seedify : String -> R.Seed
seedify =
    stringToInt >> R.initialSeed


type alias Model =
    { seed : String
    , scenario : Scenario
    }


type alias Scenario =
    { npc : Person
    , objective : Objective
    , player : Person
    , choice : Maybe Choice
    }


type alias Person =
    { name : String
    }


type alias Objective =
    { action : Action
    , object : Object
    }


scenarios : Generator Scenario
scenarios =
    R.map4 Scenario
        npc
        objectives
        (R.constant (Person "You"))
        (R.constant Nothing)


npc : Generator Person
npc =
    R.map Person names


objectives : Generator Objective
objectives =
    R.map2 Objective actions objects


type Choice
    = Yes
    | No
    | Wat


type Msg
    = UpdateSeed String
    | Choose Choice


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSeed seed ->
            { model
                | seed = seed
                , scenario = generateScenario seed
            }

        Choose choice ->
            let
                scenario =
                    model.scenario
            in
            { scenario | choice = Just choice }
                |> (\updated -> { model | scenario = updated })


stringToInt : String -> Int
stringToInt =
    String.toList
        >> List.map Char.toCode
        >> List.sum


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label []
                [ text "Seed: "
                , input
                    [ Events.onInput UpdateSeed
                    , Attr.value model.seed
                    ]
                    []
                ]
            ]
        , model.seed
            |> seedify
            |> R.step (dialogues model)
            |> Tuple.first
        ]
