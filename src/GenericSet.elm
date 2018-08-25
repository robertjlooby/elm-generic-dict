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

{-| A set of unique values. The values can be any type. The builder functions
take a comparer function that takes two values and returns an Order.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs GenericSet


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


{-| Represents a set of unique values. So `(GenericSet Int)` is a set of integers and
`(GenericSet String)` is a set of strings.
-}
type GenericSet a
    = Set_elm_builtin (GenericDict a ())


{-| Create an empty set using the given comparer.
-}
empty : (a -> a -> Order) -> GenericSet a
empty comparer =
    Set_elm_builtin (GenericDict.empty comparer)


{-| Create a set with one value using the given comparer.
-}
singleton : (a -> a -> Order) -> a -> GenericSet a
singleton comparer key =
    Set_elm_builtin (GenericDict.singleton comparer key ())


{-| Insert a value into a set.
-}
insert : a -> GenericSet a -> GenericSet a
insert key (Set_elm_builtin dict) =
    Set_elm_builtin (GenericDict.insert key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> GenericSet a -> GenericSet a
remove key (Set_elm_builtin dict) =
    Set_elm_builtin (GenericDict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : GenericSet a -> Bool
isEmpty (Set_elm_builtin dict) =
    GenericDict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> GenericSet a -> Bool
member key (Set_elm_builtin dict) =
    GenericDict.member key dict


{-| Determine the number of elements in a set.
-}
size : GenericSet a -> Int
size (Set_elm_builtin dict) =
    GenericDict.size dict


{-| Get the union of two sets. Keep all values.
Keep the comparer from the first set.
-}
union : GenericSet a -> GenericSet a -> GenericSet a
union (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (GenericDict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
Keeps the comparer from the first set.
-}
intersect : GenericSet a -> GenericSet a -> GenericSet a
intersect (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (GenericDict.intersect dict1 dict2)


{-| Get the difference between the first set and the second.
Keeps values that do not appear in the second set.
Keeps the comparer from the first set.
-}
diff : GenericSet a -> GenericSet a -> GenericSet a
diff (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (GenericDict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : GenericSet a -> List a
toList (Set_elm_builtin dict) =
    GenericDict.keys dict


{-| Convert a list into a set, removing any duplicates, using the given comparer.
-}
fromList : (a -> a -> Order) -> List a -> GenericSet a
fromList comparer list =
    List.foldl insert (empty comparer) list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> GenericSet a -> b
foldl func initialState (Set_elm_builtin dict) =
    GenericDict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> GenericSet a -> b
foldr func initialState (Set_elm_builtin dict) =
    GenericDict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates and a new
comparer.
-}
map : (b -> b -> Order) -> (a -> b) -> GenericSet a -> GenericSet b
map comparer func (Set_elm_builtin dict) =
    fromList comparer (List.map func (GenericDict.keys dict))


{-| Only keep elements that pass the given test.

    import GenericSet exposing (GenericSet)

    numbers : GenericSet Int
    numbers =
        GenericSet.fromList compare [ -2, -1, 0, 1, 2 ]

    positives : GenericSet Int
    positives =
        GenericSet.filter (\x -> x > 0) numbers


    -- positives == GenericSet.fromList compare [1,2]

-}
filter : (a -> Bool) -> GenericSet a -> GenericSet a
filter isGood (Set_elm_builtin dict) =
    Set_elm_builtin (GenericDict.filter (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> Bool) -> GenericSet a -> ( GenericSet a, GenericSet a )
partition isGood (Set_elm_builtin dict) =
    Tuple.mapBoth Set_elm_builtin Set_elm_builtin (GenericDict.partition (\key _ -> isGood key) dict)
