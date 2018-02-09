# GenericDict / GenericSet

[![Build Status](https://travis-ci.org/robertjlooby/elm-generic-dict.svg?branch=master)](https://travis-ci.org/robertjlooby/elm-generic-dict)

The core Dict/Set implementations only allows keys of `comparable` type. This
implementation builds off the core implementation but allows the user to
provide their own comparer function for ordering the keys. This allows both the
storage of arbitrary key types as well as changing the ordering of the elements
(which affects how they will be returned from `toList` for example).

The api is essentially the same as the core Dict/Set except that the "builder"
functions (`empty`, `singleton` , `fromList`) take a comparer of type
(key -> key -> Order) as their first argument.

```elm
import GenericDict exposing (GenericDict)
import String exposing (toLower)

type alias Person =
    { id : Int
    , name : String
    }

dict1 : GenericDict Person Int
dict1 =
    GenericDict.fromList
        (\person1 person2 -> compare person1.id person2.id)
        [ ( { id = 1, name = "Noah" }, 1 ), ( { id = 2, name = "Evan" }, 2 ) ]
-- Dict.fromList [({ id = 1, name = "Noah" }, 1), ({ id = 2, name = "Evan" }, 2)]

dict2 : GenericDict String Int
dict2 =
    GenericDict.fromList
        (\str1 str2 -> compare (toLower str1) (toLower str2))
        [ ( "FIRST", 1 ), ( "SECOND", 2 ), ( "First", 3 ) ]
-- Dict.fromList [("FIRST", 3), ("SECOND", 2)]

f : Maybe Int
f = GenericDict.get "FiRsT" dict2
-- Just 3
```

This builds off of the
[core Dict implementation](https://github.com/elm-lang/core/tree/3.0.0)
by Evan Czaplicki as well as the similar
[elm-all-dict](https://github.com/eeue56/elm-all-dict) by Noah Hall.
