port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (describe)
import Test.Runner.Node exposing (run)
import GenericDictTest
import GenericSetTest


main =
    run emit <|
        describe "robertjlooby/elm-generic-dict tests"
            [ GenericDictTest.tests
            , GenericSetTest.tests
            ]


port emit : ( String, Value ) -> Cmd msg
