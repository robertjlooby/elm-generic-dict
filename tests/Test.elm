module Main (..) where

import Console
import ElmTest exposing (..)
import Signal exposing (Signal)
import Task
import Test.GenericDict as GenericDict
import Test.GenericSet as GenericSet


tests : Test
tests =
  suite
    "robertjlooby/elm-generic-dict tests"
    [ GenericDict.tests
    , GenericSet.tests
    ]


port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner tests)
