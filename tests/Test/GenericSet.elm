module Test.GenericSet (tests) where

import ElmTest exposing (..)
import GenericSet


set : GenericSet.GenericSet Int
set =
  GenericSet.fromList [1..100]


setPart1 : GenericSet.GenericSet Int
setPart1 =
  GenericSet.fromList [1..50]


setPart2 : GenericSet.GenericSet Int
setPart2 =
  GenericSet.fromList [51..100]


pred : Int -> Bool
pred x =
  x <= 50


tests : Test
tests =
  let
    queryTests =
      suite "query Tests" [ test "size of set of 100 elements" <| assertEqual 100 (GenericSet.size set) ]

    filterTests =
      suite "filter Tests" [ test "Simple filter" <| assertEqual setPart1 <| GenericSet.filter pred set ]

    partitionTests =
      suite "partition Tests" [ test "Simple partition" <| assertEqual ( setPart1, setPart2 ) <| GenericSet.partition pred set ]
  in
    suite "GenericSet Tests" [ queryTests, partitionTests, filterTests ]
