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
type alias Person =
  { id : Int
  , name : String
  }

GenericDict.fromList
  (\person person' -> compare person.id person'.id)
  [{ id = 1, name = "Noah" }, { id = 2, name = "Evan" }]
-- Dict.fromList [{ id = 1, name = "Noah" }, { id = 2, name = "Evan" }]

GenericDict.fromList
  (\person person' -> compare person.name person'.name)
  [{ id = 1, name = "Noah" }, { id = 2, name = "Evan" }]
-- Dict.fromList [{ id = 2, name = "Evan" }, { id = 1, name = "Noah" }]
```

This builds off of the
[core Dict implementation](https://github.com/elm-lang/core/tree/3.0.0)
by Evan Czaplicki as well as the similar
[elm-all-dict](https://github.com/eeue56/elm-all-dict) by Noah Hall.
