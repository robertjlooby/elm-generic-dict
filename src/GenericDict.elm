module GenericDict
    exposing
        ( GenericDict
        , diff
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , get
        , insert
        , intersect
        , isEmpty
        , keys
        , map
        , member
        , merge
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        , update
        , values
        )

{-| A dictionary mapping unique keys to values. The keys can be any type. The
builder functions take a comparer function that takes two keys and returns an
Order.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs GenericDict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Combine

@docs union, intersect, diff, merge


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import RBNode exposing (RBNode)


-- DICTIONARIES
-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


{-| A dictionary of keys and values. So a `GenericDict ID User` is a dictionary
that lets you look up a `ID` and find the associated `User`.

    import GenericDict exposing (GenericDict)

    type ID
        = ID String

    compareIDs : ID -> ID -> Order
    compareIDs (ID left) (ID right) =
        compare left right

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    users : GenericDict ID User
    users =
        GenericDict.fromList compareIDs
            [ ( ID "uuid-1", User "Alice" 28 1.65 )
            , ( ID "uuid-2", User "Bob" 19 1.82 )
            , ( ID "uuid-3", User "Chuck" 33 1.75 )
            ]

-}
type GenericDict k v
    = GenericDict (RBNode k v) (k -> k -> Order)


{-| Create an empty dictionary using the given comparer.
-}
empty : (k -> k -> Order) -> GenericDict k v
empty comparer =
    GenericDict RBNode.empty comparer


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList compare [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> GenericDict k v -> Maybe v
get targetKey (GenericDict root comparer) =
    RBNode.get comparer targetKey root


{-| Determine if a key is in a dictionary.
-}
member : k -> GenericDict k v -> Bool
member key (GenericDict root comparer) =
    RBNode.member comparer key root


{-| Determine the number of key-value pairs in the dictionary.
-}
size : GenericDict k v -> Int
size (GenericDict root _) =
    RBNode.size root


{-| Determine if a dictionary is empty.

    isEmpty (empty compare) == True

-}
isEmpty : GenericDict k v -> Bool
isEmpty (GenericDict root _) =
    RBNode.isEmpty root


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> GenericDict k v -> GenericDict k v
insert key value (GenericDict root comparer) =
    GenericDict (RBNode.insert comparer key value root) comparer


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> GenericDict k v -> GenericDict k v
remove key (GenericDict root comparer) =
    GenericDict (RBNode.remove comparer key root) comparer


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> GenericDict k v -> GenericDict k v
update targetKey alter (GenericDict root comparer) =
    GenericDict (RBNode.update comparer targetKey alter root) comparer


{-| Create a dictionary with one key-value pair, using the given comparer.
-}
singleton : (k -> k -> Order) -> k -> v -> GenericDict k v
singleton comparer key value =
    -- Root node is always Black
    GenericDict (RBNode.singleton key value) comparer



-- COMBINE


{-| Combine two dictionaries.
If there is a collision, preference is given to the first dictionary.
Keep the comparer from the first dictionary.
-}
union : GenericDict k v -> GenericDict k v -> GenericDict k v
union (GenericDict t1 comparer) (GenericDict t2 _) =
    GenericDict (RBNode.union comparer t1 t2) comparer


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
Keep the comparer from the first dictionary.
-}
intersect : GenericDict k v -> GenericDict k v -> GenericDict k v
intersect (GenericDict t1 comparer) (GenericDict t2 _) =
    GenericDict (RBNode.intersect comparer t1 t2) comparer


{-| Keep a key-value pair when its key does not appear in the second dictionary.
Keep the comparer from the first dictionary.
-}
diff : GenericDict k a -> GenericDict k b -> GenericDict k a
diff (GenericDict t1 comparer) (GenericDict t2 _) =
    GenericDict (RBNode.diff comparer t1 t2) comparer


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> GenericDict k a
    -> GenericDict k b
    -> result
    -> result
merge leftStep bothStep rightStep (GenericDict leftRoot comparer) (GenericDict rightRoot _) initialResult =
    RBNode.merge comparer leftStep bothStep rightStep leftRoot rightRoot initialResult



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> GenericDict k a -> GenericDict k b
map func (GenericDict root comparer) =
    GenericDict (RBNode.map func root) comparer


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import GenericDict exposing (GenericDict)

    getAges : GenericDict String User -> List String
    getAges users =
        GenericDict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages


    -- getAges users == [33,19,28]

-}
foldl : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldl func acc (GenericDict root _) =
    RBNode.foldl func acc root


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import GenericDict exposing (GenericDict)

    getAges : GenericDict String User -> List String
    getAges users =
        GenericDict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages


    -- getAges users == [28,19,33]

-}
foldr : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldr func acc (GenericDict root _) =
    RBNode.foldr func acc root


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> GenericDict k v -> GenericDict k v
filter isGood (GenericDict root comparer) =
    GenericDict (RBNode.filter comparer isGood root) comparer


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (k -> v -> Bool) -> GenericDict k v -> ( GenericDict k v, GenericDict k v )
partition isGood (GenericDict root comparer) =
    let
        ( leftRoot, rightRoot ) =
            RBNode.partition comparer isGood root
    in
    ( GenericDict leftRoot comparer, GenericDict rightRoot comparer )



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList compare [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : GenericDict k v -> List k
keys (GenericDict root _) =
    RBNode.keys root


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList compare [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : GenericDict k v -> List v
values (GenericDict root _) =
    RBNode.values root


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : GenericDict k v -> List ( k, v )
toList (GenericDict root _) =
    RBNode.toList root


{-| Convert an association list into a dictionary using the given comparer.
-}
fromList : (k -> k -> Order) -> List ( k, v ) -> GenericDict k v
fromList comparer assocs =
    GenericDict (RBNode.fromList comparer assocs) comparer
