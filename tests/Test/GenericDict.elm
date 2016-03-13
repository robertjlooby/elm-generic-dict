module Test.GenericDict (tests) where

import ElmTest exposing (..)
import GenericDict


animals : GenericDict.GenericDict String String
animals =
  GenericDict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


tests : Test
tests =
  let
    buildTests =
      suite
        "build Tests"
        [ test "empty" <| assertEqual (GenericDict.fromList []) (GenericDict.empty)
        , test "singleton" <| assertEqual (GenericDict.fromList [ ( "k", "v" ) ]) (GenericDict.singleton "k" "v")
        , test "insert" <| assertEqual (GenericDict.fromList [ ( "k", "v" ) ]) (GenericDict.insert "k" "v" GenericDict.empty)
        , test "insert replace" <| assertEqual (GenericDict.fromList [ ( "k", "vv" ) ]) (GenericDict.insert "k" "vv" (GenericDict.singleton "k" "v"))
        , test "update" <| assertEqual (GenericDict.fromList [ ( "k", "vv" ) ]) (GenericDict.update "k" (\v -> Just "vv") (GenericDict.singleton "k" "v"))
        , test "update Nothing" <| assertEqual GenericDict.empty (GenericDict.update "k" (\v -> Nothing) (GenericDict.singleton "k" "v"))
        , test "remove" <| assertEqual GenericDict.empty (GenericDict.remove "k" (GenericDict.singleton "k" "v"))
        , test "remove not found" <| assertEqual (GenericDict.singleton "k" "v") (GenericDict.remove "kk" (GenericDict.singleton "k" "v"))
        ]

    queryTests =
      suite
        "query Tests"
        [ test "member 1" <| assertEqual True (GenericDict.member "Tom" animals)
        , test "member 2" <| assertEqual False (GenericDict.member "Spike" animals)
        , test "get 1" <| assertEqual (Just "cat") (GenericDict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (GenericDict.get "Spike" animals)
        , test "size of empty dictionary" <| assertEqual 0 (GenericDict.size GenericDict.empty)
        , test "size of example dictionary" <| assertEqual 2 (GenericDict.size animals)
        ]

    combineTests =
      suite
        "combine Tests"
        [ test "union" <| assertEqual animals (GenericDict.union (GenericDict.singleton "Jerry" "mouse") (GenericDict.singleton "Tom" "cat"))
        , test "union collison" <| assertEqual (GenericDict.singleton "Tom" "cat") (GenericDict.union (GenericDict.singleton "Tom" "cat") (GenericDict.singleton "Tom" "mouse"))
        , test "intersect" <| assertEqual (GenericDict.singleton "Tom" "cat") (GenericDict.intersect animals (GenericDict.singleton "Tom" "cat"))
        , test "diff" <| assertEqual (GenericDict.singleton "Jerry" "mouse") (GenericDict.diff animals (GenericDict.singleton "Tom" "cat"))
        ]

    transformTests =
      suite
        "transform Tests"
        [ test "filter" <| assertEqual (GenericDict.singleton "Tom" "cat") (GenericDict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual ( GenericDict.singleton "Tom" "cat", GenericDict.singleton "Jerry" "mouse" ) (GenericDict.partition (\k v -> k == "Tom") animals)
        ]
  in
    suite
      "GenericDict Tests"
      [ buildTests
      , queryTests
      , combineTests
      , transformTests
      ]
