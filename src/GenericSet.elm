module GenericSet
    exposing
        ( GenericSet
        , diff
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , insert
        , intersect
        , isEmpty
        , map
        , member
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import GenericDict exposing (GenericDict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type GenericSet a
    = GenericSet (GenericDict a ())


{-| Create an empty set.
-}
empty : (a -> a -> Order) -> GenericSet a
empty comparer =
    GenericSet (GenericDict.empty comparer)


{-| Create a set with one value.
-}
singleton : (a -> a -> Order) -> a -> GenericSet a
singleton comparer key =
    GenericSet (GenericDict.singleton comparer key ())


{-| Insert a value into a set.
-}
insert : a -> GenericSet a -> GenericSet a
insert key (GenericSet dict) =
    GenericSet (GenericDict.insert key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> GenericSet a -> GenericSet a
remove key (GenericSet dict) =
    GenericSet (GenericDict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : GenericSet a -> Bool
isEmpty (GenericSet dict) =
    GenericDict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> GenericSet a -> Bool
member key (GenericSet dict) =
    GenericDict.member key dict


{-| Determine the number of elements in a set.
-}
size : GenericSet a -> Int
size (GenericSet dict) =
    GenericDict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : GenericSet a -> GenericSet a -> GenericSet a
union (GenericSet dict1) (GenericSet dict2) =
    GenericSet (GenericDict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : GenericSet a -> GenericSet a -> GenericSet a
intersect (GenericSet dict1) (GenericSet dict2) =
    GenericSet (GenericDict.intersect dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : GenericSet a -> GenericSet a -> GenericSet a
diff (GenericSet dict1) (GenericSet dict2) =
    GenericSet (GenericDict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : GenericSet a -> List a
toList (GenericSet dict) =
    GenericDict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> a -> Order) -> List a -> GenericSet a
fromList comparer list =
    List.foldl insert (empty comparer) list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> GenericSet a -> b
foldl func initialState (GenericSet dict) =
    GenericDict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> GenericSet a -> b
foldr func initialState (GenericSet dict) =
    GenericDict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (b -> b -> Order) -> (a -> b) -> GenericSet a -> GenericSet b
map comparer func (GenericSet dict) =
    fromList comparer (List.map func (GenericDict.keys dict))


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers


    -- positives == Set.fromList [1,2]

-}
filter : (a -> Bool) -> GenericSet a -> GenericSet a
filter isGood (GenericSet dict) =
    GenericSet (GenericDict.filter (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> Bool) -> GenericSet a -> ( GenericSet a, GenericSet a )
partition isGood (GenericSet dict) =
    let
        ( dict1, dict2 ) =
            GenericDict.partition (\key _ -> isGood key) dict
    in
    ( GenericSet dict1, GenericSet dict2 )
