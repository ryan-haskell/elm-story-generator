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


verbs : Generator String
verbs =
    R.uniform
        "steal"
        [ "slay"
        , "save"
        , "find"
        , "destroy"
        , "win"
        , "protect"
        ]


objects : Generator String
objects =
    R.uniform
        "treasure"
        [ "dragon"
        , "princess"
        , "amulet"
        , "portal"
        , "war"
        , "castle"
        ]


capitalize : String -> String
capitalize str =
    String.uncons str
        |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
        |> Maybe.withDefault str


intros : Generator (Html msg)
intros =
    R.map2 say
        names
        quests


say : String -> String -> Html msg
say name line =
    p [] [ span [] [ strong [] [ text (name ++ ": ") ], text line ] ]


dialogues : Info msg -> Generator (Html msg)
dialogues info =
    R.map2 (\intro choice -> div [] [ intro, choice ])
        intros
        (choices info)


choices : Info msg -> Generator (Html msg)
choices info =
    R.map3
        (\yes no wat ->
            case info.choice of
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
                        [ button [ Events.onClick (info.choose Yes) ] [ text yes ]
                        , span [] [ text "  " ]
                        , button [ Events.onClick (info.choose No) ] [ text no ]
                        , span [] [ text "  " ]
                        , button [ Events.onClick (info.choose Wat) ] [ text wat ]
                        ]
        )
        affirmativeResponses
        negativeResponses
        confusedResponses


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


confusedResponses : Generator String
confusedResponses =
    R.uniform
        "What?"
        [ "Why me?"
        , "That doesn't make sense."
        , "Who are you?"
        , "Huh?"
        ]


affirmativeResponses : Generator String
affirmativeResponses =
    R.uniform
        "How can I help?"
        [ "Okay!"
        , "You got it."
        , "I'm on it!"
        ]


negativeResponses : Generator String
negativeResponses =
    R.uniform
        "Eh. Not really."
        [ "I'd rather not."
        , "Not today!"
        , "Me no want do."
        , "Nah."
        , "Haha, no."
        ]


quests : Generator String
quests =
    R.map4
        (\imploration subject verb object ->
            String.join " "
                [ imploration ++ ","
                , subject ++ "!"
                , "You"
                , "must"
                , verb
                , "the"
                , object
                ]
                ++ "!"
                |> capitalize
        )
        greetings
        subjects
        verbs
        objects


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { seed = "123"
    , choice = Nothing
    }


type alias Model =
    { seed : String
    , choice : Maybe Choice
    }


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
            { model | seed = seed }

        Choose choice ->
            { model | choice = Just choice }


stringToInt : String -> Int
stringToInt =
    String.toList
        >> List.map Char.toCode
        >> List.sum


type alias Info msg =
    { choice : Maybe Choice
    , choose : Choice -> msg
    }


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
            |> stringToInt
            |> R.initialSeed
            |> R.step (dialogues { choice = model.choice, choose = Choose })
            |> Tuple.first
        ]
