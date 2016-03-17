module Test.GenericDict (tests) where

import ElmTest exposing (..)
import GenericDict


animals : GenericDict.GenericDict String String
animals =
  GenericDict.fromList compare [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


tests : Test
tests =
  let
    buildTests =
      suite
        "build Tests"
        [ test "empty" <| assertEqual (GenericDict.fromList compare []) (GenericDict.empty compare)
        , test "singleton" <| assertEqual (GenericDict.fromList compare [ ( "k", "v" ) ]) (GenericDict.singleton compare "k" "v")
        , test "insert" <| assertEqual (GenericDict.fromList compare [ ( "k", "v" ) ]) (GenericDict.insert "k" "v" <| GenericDict.empty compare)
        , test "insert replace" <| assertEqual (GenericDict.fromList compare [ ( "k", "vv" ) ]) (GenericDict.insert "k" "vv" (GenericDict.singleton compare "k" "v"))
        , test "update" <| assertEqual (GenericDict.fromList compare [ ( "k", "vv" ) ]) (GenericDict.update "k" (\v -> Just "vv") (GenericDict.singleton compare "k" "v"))
        , test "update Nothing" <| assertEqual (GenericDict.empty compare) (GenericDict.update "k" (\v -> Nothing) (GenericDict.singleton compare "k" "v"))
        , test "remove" <| assertEqual (GenericDict.empty compare) (GenericDict.remove "k" (GenericDict.singleton compare "k" "v"))
        , test "remove not found" <| assertEqual (GenericDict.singleton compare "k" "v") (GenericDict.remove "kk" (GenericDict.singleton compare "k" "v"))
        ]

    queryTests =
      suite
        "query Tests"
        [ test "member 1" <| assertEqual True (GenericDict.member "Tom" animals)
        , test "member 2" <| assertEqual False (GenericDict.member "Spike" animals)
        , test "get 1" <| assertEqual (Just "cat") (GenericDict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (GenericDict.get "Spike" animals)
        , test "size of empty dictionary" <| assertEqual 0 (GenericDict.size <| GenericDict.empty compare)
        , test "size of example dictionary" <| assertEqual 2 (GenericDict.size animals)
        ]

    combineTests =
      suite
        "combine Tests"
        [ test "union" <| assertEqual animals (GenericDict.union (GenericDict.singleton compare "Jerry" "mouse") (GenericDict.singleton compare "Tom" "cat"))
        , test "union collison" <| assertEqual (GenericDict.singleton compare "Tom" "cat") (GenericDict.union (GenericDict.singleton compare "Tom" "cat") (GenericDict.singleton compare "Tom" "mouse"))
        , test "intersect" <| assertEqual (GenericDict.singleton compare "Tom" "cat") (GenericDict.intersect animals (GenericDict.singleton compare "Tom" "cat"))
        , test "diff" <| assertEqual (GenericDict.singleton compare "Jerry" "mouse") (GenericDict.diff animals (GenericDict.singleton compare "Tom" "cat"))
        ]

    transformTests =
      suite
        "transform Tests"
        [ test "filter" <| assertEqual (GenericDict.singleton compare "Tom" "cat") (GenericDict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual ( GenericDict.singleton compare "Tom" "cat", GenericDict.singleton compare "Jerry" "mouse" ) (GenericDict.partition (\k v -> k == "Tom") animals)
        ]
  in
    suite
      "GenericDict Tests"
      [ buildTests
      , queryTests
      , combineTests
      , transformTests
      ]
