module Test exposing (..)

import ElmTest exposing (..)
import Test.GenericDict as GenericDict
import Test.GenericSet as GenericSet


tests : Test
tests =
  suite
    "robertjlooby/elm-generic-dict tests"
    [ GenericDict.tests
    , GenericSet.tests
    ]


main : Program Never
main =
    runSuite tests
