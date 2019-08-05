module Main exposing (main)

import Html exposing (..)
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


dialogues : Generator (Html msg)
dialogues =
    R.map2 (\name line -> div [] [ span [] [ strong [] [ text (name ++ ": ") ], text line ] ])
        names
        quests


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


main =
    123456890
        |> R.initialSeed
        |> R.step dialogues
        |> Tuple.first
