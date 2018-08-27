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


## Attention

You have to use same comparator for dictionaries or sets of same data.
This requirement is needed to keep correct working of operations such as
get/insert/update/remove/etc.

```elm
-- BAD unexpected result


reversedCompare : comparable -> comparable -> Order
reversedCompare a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


unexpectedResult : GenericDict String Int
unexpectedResult =
    GenericDict.union
        (GenericDict.fromList reversedCompare [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ), ( 4, "d" ) ])
        (GenericDict.fromList compare [ ( 3, "cc" ), ( 4, "dd" ), ( 5, "ee" ) ])


unexpectedEqual : Bool
unexpectedEqual =
    -- True
    GenericDict.toList unexpectedResult == [ ( 3, "cc" ), ( 4, "d" ), ( 5, "ee" ), ( 3, "c" ), ( 2, "b" ), ( 1, "a" ) ]



-- GOOD expected result


expectedResult : GenericDict String Int
expectedResult =
    GenericDict.union
        (GenericDict.fromList compare [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ), ( 4, "d" ) ])
        (GenericDict.fromList compare [ ( 3, "cc" ), ( 4, "dd" ), ( 5, "ee" ) ])


expectedEqual : Bool
expectedEqual =
    -- True
    GenericDict.toList expectedResult == [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ), ( 4, "d" ), ( 5, "ee" ) ]

```

---

This builds off of the [core Dict implementation][elm-core] by Evan Czaplicki.

[elm-core]: https://github.com/elm/core/tree/1.0.0
