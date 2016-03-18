module GenericSet (GenericSet, empty, singleton, insert, remove, isEmpty, member, size, foldl, foldr, map, filter, partition, union, intersect, diff, toList, fromList) where

{-| A set of unique values. The values can be any type. The builder functions
take a comparer function that takes two values and returns an Order.

Insert, remove, and query operations all take *O(log n)* time. GenericSet equality with
`(==)` is unreliable and should not be used.

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

import GenericDict
import Set as CoreSet


{-| Represents a set of unique values. So `(GenericSet Int)` is a set of integers and
`(GenericSet String)` is a set of strings.
-}
type GenericSet t
  = Set_elm_builtin (GenericDict.GenericDict t ())


{-| Create an empty set using the given comparer.
-}
empty : (a -> a -> Order) -> GenericSet a
empty comparer =
  Set_elm_builtin <| GenericDict.empty comparer


{-| Create a set with one value using the given comparer.
-}
singleton : (a -> a -> Order) -> a -> GenericSet a
singleton comparer k =
  Set_elm_builtin <| GenericDict.singleton comparer k ()


{-| Insert a value into a set.
-}
insert : a -> GenericSet a -> GenericSet a
insert k (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> GenericSet a -> GenericSet a
remove k (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : GenericSet a -> Bool
isEmpty (Set_elm_builtin d) =
  GenericDict.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> GenericSet a -> Bool
member k (Set_elm_builtin d) =
  GenericDict.member k d


{-| Determine the number of elements in a set.
-}
size : GenericSet a -> Int
size (Set_elm_builtin d) =
  GenericDict.size d


{-| Get the union of two sets. Keep all values. Keep the comparer from the
first set.
-}
union : GenericSet a -> GenericSet a -> GenericSet a
union (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
Keeps the comparer from the first set.
-}
intersect : GenericSet a -> GenericSet a -> GenericSet a
intersect (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set. Keeps the comparer from the first set.
-}
diff : GenericSet a -> GenericSet a -> GenericSet a
diff (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.diff d1 d2


{-| Convert a set into a list, sorted by the comparer.
-}
toList : GenericSet a -> List a
toList (Set_elm_builtin d) =
  GenericDict.keys d


{-| Convert a list into a set, removing any duplicates, using the given
comparer.
-}
fromList : (a -> a -> Order) -> List a -> GenericSet a
fromList comparer xs =
  List.foldl insert (empty comparer) xs


{-| Fold over the values in a set, in order from lowest to highest (by the
comparer).
-}
foldl : (a -> b -> b) -> b -> GenericSet a -> b
foldl f b (Set_elm_builtin d) =
  GenericDict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest (by the
comparer).
-}
foldr : (a -> b -> b) -> b -> GenericSet a -> b
foldr f b (Set_elm_builtin d) =
  GenericDict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates and a new
comparer.
-}
map : (b -> b -> Order) -> (a -> b) -> GenericSet a -> GenericSet b
map comparer f s =
  fromList comparer (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> GenericSet a -> GenericSet a
filter p (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> GenericSet a -> ( GenericSet a, GenericSet a )
partition p (Set_elm_builtin d) =
  let
    ( p1, p2 ) =
      GenericDict.partition (\k _ -> p k) d
  in
    ( Set_elm_builtin p1, Set_elm_builtin p2 )
