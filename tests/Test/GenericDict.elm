module Test.GenericDict exposing (tests)

import ElmTest exposing (..)
import GenericDict


compare' : comparable -> comparable -> Order
compare' a a' =
  case compare a a' of
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
      suite
        "build Tests"
        [ test
            "empty"
            <| assertEqual
                (GenericDict.fromList compare [])
                (GenericDict.empty compare)
        , test
            "singleton"
            <| assertEqual
                (GenericDict.fromList compare [ ( "k", "v" ) ])
                (GenericDict.singleton compare "k" "v")
        , test
            "insert"
            <| assertEqual
                (GenericDict.fromList compare [ ( "k", "v" ) ])
                (GenericDict.insert "k" "v" <| GenericDict.empty compare)
        , test
            "insert replace"
            <| assertEqual
                (GenericDict.fromList compare [ ( "k", "vv" ) ])
                (GenericDict.insert "k" "vv" (GenericDict.singleton compare "k" "v"))
        , test
            "update"
            <| assertEqual
                (GenericDict.fromList compare [ ( "k", "vv" ) ])
                (GenericDict.update "k" (\v -> Just "vv") (GenericDict.singleton compare "k" "v"))
        , test
            "update Nothing"
            <| assertEqual
                (GenericDict.empty compare)
                (GenericDict.update "k" (\v -> Nothing) (GenericDict.singleton compare "k" "v"))
        , test
            "remove"
            <| assertEqual
                (GenericDict.empty compare)
                (GenericDict.remove "k" (GenericDict.singleton compare "k" "v"))
        , test "remove not found"
            <| assertEqual
                (GenericDict.singleton compare "k" "v")
                (GenericDict.remove "kk" (GenericDict.singleton compare "k" "v"))
        ]

    queryTests =
      suite
        "query Tests"
        [ test "member 1" <| assertEqual True (GenericDict.member "Tom" animals)
        , test "member 2" <| assertEqual False (GenericDict.member "Spike" animals)
        , test
            "member uses given comparer"
            <| let
                comparer a a' =
                  compare a.id a'.id

                dict =
                  GenericDict.fromList comparer [ ( { id = 1, name = "thing" }, () ) ]
               in
                assertEqual
                  True
                  (GenericDict.member { id = 1, name = "other" } dict)
        , test "get 1" <| assertEqual (Just "cat") (GenericDict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (GenericDict.get "Spike" animals)
        , test "size of empty dictionary" <| assertEqual 0 (GenericDict.size <| GenericDict.empty compare)
        , test "size of example dictionary" <| assertEqual 2 (GenericDict.size animals)
        ]

    combineTests =
      let
        dict1 =
          GenericDict.fromList compare' [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ), ( 4, "d" ) ]

        dict2 =
          GenericDict.fromList compare [ ( 3, "cc" ), ( 4, "dd" ), ( 5, "ee" ) ]
      in
        suite
          "combine Tests"
          [ test
              "union uses first groups comparer (and values in a collision)"
              <| assertEqual
                  [ ( 5, "ee" ), ( 4, "d" ), ( 3, "c" ), ( 2, "b" ), ( 1, "a" ) ]
                  (GenericDict.union dict1 dict2 |> GenericDict.toList)
          , test
              "intersect uses first groups comparer (and values in a collision)"
              <| assertEqual
                  [ ( 4, "d" ), ( 3, "c" ) ]
                  (GenericDict.intersect dict1 dict2 |> GenericDict.toList)
          , test
              "diff uses first groups comparer"
              <| assertEqual
                  [ ( 2, "b" ), ( 1, "a" ) ]
                  (GenericDict.diff dict1 dict2 |> GenericDict.toList)
          ]

    keyValueTests =
      suite
        "key/value Tests"
        [ test "keys"
            <| assertEqual [ "Jerry", "Tom" ] (GenericDict.keys animals)
        , test "values"
            <| assertEqual [ "mouse", "cat" ] (GenericDict.values animals)
        ]

    transformTests =
      suite
        "transform Tests"
        [ test
            "map"
            <| assertEqual
                [ ( "Jerry", "the mouse" ), ( "Tom", "the cat" ) ]
                (GenericDict.map (\k v -> "the " ++ v) animals |> GenericDict.toList)
        , test
            "foldl"
            <| assertEqual
                "xJerrymouseTomcat"
                (GenericDict.foldl (\k v acc -> acc ++ k ++ v) "x" animals)
        , test
            "foldr"
            <| assertEqual
                "xTomcatJerrymouse"
                (GenericDict.foldr (\k v acc -> acc ++ k ++ v) "x" animals)
        , test
            "filter"
            <| assertEqual
                (GenericDict.singleton compare "Tom" "cat")
                (GenericDict.filter (\k v -> k == "Tom") animals)
        , test
            "partition"
            <| assertEqual
                ( GenericDict.singleton compare "Tom" "cat", GenericDict.singleton compare "Jerry" "mouse" )
                (GenericDict.partition (\k v -> k == "Tom") animals)
        ]
  in
    suite
      "GenericDict Tests"
      [ buildTests
      , queryTests
      , combineTests
      , keyValueTests
      , transformTests
      ]
