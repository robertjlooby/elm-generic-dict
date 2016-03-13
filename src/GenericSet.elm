module GenericSet (GenericSet, empty, singleton, insert, remove, isEmpty, member, size, foldl, foldr, map, filter, partition, union, intersect, diff, toList, fromList) where

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

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


{-| Create an empty set.
-}
empty : GenericSet a
empty =
  Set_elm_builtin GenericDict.empty


{-| Create a set with one value.
-}
singleton : comparable -> GenericSet comparable
singleton k =
  Set_elm_builtin <| GenericDict.singleton k ()


{-| Insert a value into a set.
-}
insert : comparable -> GenericSet comparable -> GenericSet comparable
insert k (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : comparable -> GenericSet comparable -> GenericSet comparable
remove k (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : GenericSet a -> Bool
isEmpty (Set_elm_builtin d) =
  GenericDict.isEmpty d


{-| Determine if a value is in a set.
-}
member : comparable -> GenericSet comparable -> Bool
member k (Set_elm_builtin d) =
  GenericDict.member k d


{-| Determine the number of elements in a set.
-}
size : GenericSet a -> Int
size (Set_elm_builtin d) =
  GenericDict.size d


{-| Get the union of two sets. Keep all values.
-}
union : GenericSet comparable -> GenericSet comparable -> GenericSet comparable
union (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : GenericSet comparable -> GenericSet comparable -> GenericSet comparable
intersect (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : GenericSet comparable -> GenericSet comparable -> GenericSet comparable
diff (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| GenericDict.diff d1 d2


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : GenericSet comparable -> List comparable
toList (Set_elm_builtin d) =
  GenericDict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List comparable -> GenericSet comparable
fromList xs =
  List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (comparable -> b -> b) -> b -> GenericSet comparable -> b
foldl f b (Set_elm_builtin d) =
  GenericDict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (comparable -> b -> b) -> b -> GenericSet comparable -> b
foldr f b (Set_elm_builtin d) =
  GenericDict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (comparable -> comparable') -> GenericSet comparable -> GenericSet comparable'
map f s =
  fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (comparable -> Bool) -> GenericSet comparable -> GenericSet comparable
filter p (Set_elm_builtin d) =
  Set_elm_builtin <| GenericDict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (comparable -> Bool) -> GenericSet comparable -> ( GenericSet comparable, GenericSet comparable )
partition p (Set_elm_builtin d) =
  let
    ( p1, p2 ) =
      GenericDict.partition (\k _ -> p k) d
  in
    ( Set_elm_builtin p1, Set_elm_builtin p2 )
