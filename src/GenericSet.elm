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

import RBNode exposing (RBNode)


{-| Represents a set of unique values. So `(GenericSet Int)` is a set of integers and
`(GenericSet String)` is a set of strings.
-}
type GenericSet a
    = Set_elm_builtin (RBNode a ()) (a -> a -> Order)


{-| Create an empty set using the given comparer.
-}
empty : (a -> a -> Order) -> GenericSet a
empty comparer =
    Set_elm_builtin RBNode.empty comparer


{-| Create a set with one value using the given comparer.
-}
singleton : (a -> a -> Order) -> a -> GenericSet a
singleton comparer key =
    Set_elm_builtin (RBNode.singleton key ()) comparer


{-| Insert a value into a set.
-}
insert : a -> GenericSet a -> GenericSet a
insert key (Set_elm_builtin root comparer) =
    Set_elm_builtin (RBNode.insert comparer key () root) comparer


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> GenericSet a -> GenericSet a
remove key (Set_elm_builtin root comparer) =
    Set_elm_builtin (RBNode.remove comparer key root) comparer


{-| Determine if a set is empty.
-}
isEmpty : GenericSet a -> Bool
isEmpty (Set_elm_builtin root _) =
    RBNode.isEmpty root


{-| Determine if a value is in a set.
-}
member : a -> GenericSet a -> Bool
member key (Set_elm_builtin root comparer) =
    RBNode.member comparer key root


{-| Determine the number of elements in a set.
-}
size : GenericSet a -> Int
size (Set_elm_builtin root _) =
    RBNode.size root


{-| Get the union of two sets. Keep all values.
Keep the comparer from the first set.
-}
union : GenericSet a -> GenericSet a -> GenericSet a
union (Set_elm_builtin t1 comparer) (Set_elm_builtin t2 _) =
    Set_elm_builtin (RBNode.union comparer t1 t2) comparer


{-| Get the intersection of two sets. Keeps values that appear in both sets.
Keeps the comparer from the first set.
-}
intersect : GenericSet a -> GenericSet a -> GenericSet a
intersect (Set_elm_builtin t1 comparer) (Set_elm_builtin t2 _) =
    Set_elm_builtin (RBNode.intersect comparer t1 t2) comparer


{-| Get the difference between the first set and the second.
Keeps values that do not appear in the second set.
Keeps the comparer from the first set.
-}
diff : GenericSet a -> GenericSet a -> GenericSet a
diff (Set_elm_builtin t1 comparer) (Set_elm_builtin t2 _) =
    Set_elm_builtin (RBNode.diff comparer t1 t2) comparer


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : GenericSet a -> List a
toList (Set_elm_builtin root _) =
    RBNode.keys root


{-| Convert a list into a set, removing any duplicates, using the given comparer.
-}
fromList : (a -> a -> Order) -> List a -> GenericSet a
fromList comparer list =
    List.foldl insert (empty comparer) list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> GenericSet a -> b
foldl func initialState (Set_elm_builtin root _) =
    RBNode.foldl (\key _ state -> func key state) initialState root


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> GenericSet a -> b
foldr func initialState (Set_elm_builtin root _) =
    RBNode.foldr (\key _ state -> func key state) initialState root


{-| Map a function onto a set, creating a new set with no duplicates and a new
comparer.
-}
map : (b -> b -> Order) -> (a -> b) -> GenericSet a -> GenericSet b
map comparer func (Set_elm_builtin root _) =
    fromList comparer (List.map func (RBNode.keys root))


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
filter isGood (Set_elm_builtin root comparer) =
    Set_elm_builtin (RBNode.filter comparer (\key _ -> isGood key) root) comparer


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> Bool) -> GenericSet a -> ( GenericSet a, GenericSet a )
partition isGood (Set_elm_builtin root comparer) =
    let
        ( leftRoot, rightRoot ) =
            RBNode.partition comparer (\key _ -> isGood key) root
    in
    ( Set_elm_builtin leftRoot comparer, Set_elm_builtin rightRoot comparer )
