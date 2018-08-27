module GenericDictTest exposing (tests)

import Expect exposing (Expectation)
import GenericDict
import Test exposing (Test, describe, skip, test)


compare2 : comparable -> comparable -> Order
compare2 a b =
    case compare a b of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


animals : GenericDict.GenericDict String String
animals =
    GenericDict.fromList compare [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <|
                    \() ->
                        GenericDict.empty compare
                            |> Expect.equal (GenericDict.fromList compare [])
                , test "singleton" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> Expect.equal (GenericDict.fromList compare [ ( "k", "v" ) ])
                , test "insert" <|
                    \() ->
                        GenericDict.empty compare
                            |> GenericDict.insert "k" "v"
                            |> Expect.equal (GenericDict.fromList compare [ ( "k", "v" ) ])
                , test "insert replace" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> GenericDict.insert "k" "vv"
                            |> Expect.equal (GenericDict.fromList compare [ ( "k", "vv" ) ])
                , test "update" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> GenericDict.update "k" (\v -> Just "vv")
                            |> Expect.equal (GenericDict.fromList compare [ ( "k", "vv" ) ])
                , test "update Nothing" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> GenericDict.update "k" (\v -> Nothing)
                            |> Expect.equal (GenericDict.empty compare)
                , test "remove" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> GenericDict.remove "k"
                            |> Expect.equal (GenericDict.empty compare)
                , test "remove not found" <|
                    \() ->
                        GenericDict.singleton compare "k" "v"
                            |> GenericDict.remove "kk"
                            |> Expect.equal (GenericDict.singleton compare "k" "v")
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <|
                    \() ->
                        GenericDict.member "Tom" animals
                            |> Expect.equal True
                , test "member 2" <|
                    \() ->
                        GenericDict.member "Spike" animals
                            |> Expect.equal False
                , test "member uses given comparer" <|
                    \() ->
                        let
                            comparer a b =
                                compare a.id b.id

                            dict =
                                GenericDict.fromList comparer [ ( { id = 1, name = "thing" }, () ) ]
                        in
                        GenericDict.member { id = 1, name = "other" } dict
                            |> Expect.equal True
                , test "get 1" <|
                    \() ->
                        GenericDict.get "Tom" animals
                            |> Expect.equal (Just "cat")
                , test "get 2" <|
                    \() ->
                        GenericDict.get "Spike" animals
                            |> Expect.equal Nothing
                , test "size of empty dictionary" <|
                    \() ->
                        GenericDict.empty compare
                            |> GenericDict.size
                            |> Expect.equal 0
                , test "size of example dictionary" <|
                    \() ->
                        GenericDict.size animals
                            |> Expect.equal 2
                ]

        combineTests =
            let
                dict1 =
                    GenericDict.fromList compare2 [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ), ( 4, "d" ) ]

                dict2 =
                    GenericDict.fromList compare [ ( 3, "cc" ), ( 4, "dd" ), ( 5, "ee" ) ]
            in
            describe "combine Tests"
                [ skip <|
                    test "union uses first groups comparer (and values in a collision)" <|
                        \() ->
                            GenericDict.union dict1 dict2
                                |> GenericDict.toList
                                |> Expect.equal [ ( 5, "ee" ), ( 4, "d" ), ( 3, "c" ), ( 2, "b" ), ( 1, "a" ) ]
                , skip <|
                    test "intersect uses first groups comparer (and values in a collision)" <|
                        \() ->
                            GenericDict.intersect dict1 dict2
                                |> GenericDict.toList
                                |> Expect.equal [ ( 4, "d" ), ( 3, "c" ) ]
                , skip <|
                    test "diff uses first groups comparer" <|
                        \() ->
                            GenericDict.diff dict1 dict2
                                |> GenericDict.toList
                                |> Expect.equal [ ( 2, "b" ), ( 1, "a" ) ]
                ]

        keyValueTests =
            describe "key/value Tests"
                [ test "keys" <|
                    \() ->
                        GenericDict.keys animals
                            |> Expect.equal [ "Jerry", "Tom" ]
                , test "values" <|
                    \() ->
                        GenericDict.values animals
                            |> Expect.equal [ "mouse", "cat" ]
                ]

        transformTests =
            describe "transform Tests"
                [ test "map" <|
                    \() ->
                        GenericDict.map (\k v -> "the " ++ v) animals
                            |> GenericDict.toList
                            |> Expect.equal [ ( "Jerry", "the mouse" ), ( "Tom", "the cat" ) ]
                , test "foldl" <|
                    \() ->
                        GenericDict.foldl (\k v acc -> acc ++ k ++ v) "x" animals
                            |> Expect.equal "xJerrymouseTomcat"
                , test "foldr" <|
                    \() ->
                        GenericDict.foldr (\k v acc -> acc ++ k ++ v) "x" animals
                            |> Expect.equal "xTomcatJerrymouse"
                , test "filter" <|
                    \() ->
                        GenericDict.filter (\k v -> k == "Tom") animals
                            |> Expect.equal (GenericDict.singleton compare "Tom" "cat")
                , test "partition" <|
                    \() ->
                        GenericDict.partition (\k v -> k == "Tom") animals
                            |> Expect.equal ( GenericDict.singleton compare "Tom" "cat", GenericDict.singleton compare "Jerry" "mouse" )
                ]
    in
    describe "GenericDict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , keyValueTests
        , transformTests
        ]
