module Main exposing (..)

import GenericDictTest
import GenericSetTest
import Test exposing (..)


suite : Test
suite =
    describe "robertjlooby/elm-generic-dict tests"
        [ GenericDictTest.tests
        , GenericSetTest.tests
        ]
