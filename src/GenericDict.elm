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
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        , update
        , values
        )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

-- DICTIONARIES
-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


type GenericDict k v
    = GenericDict (k -> k -> Order) (RBNode k v)


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type RBNode k v
    = RBNode_elm_builtin NColor k v (RBNode k v) (RBNode k v)
    | RBEmpty_elm_builtin


{-| Create an empty dictionary.
-}
empty : (k -> k -> Order) -> GenericDict k v
empty comparer =
    GenericDict comparer RBEmpty_elm_builtin


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> GenericDict k v -> Maybe v
get targetKey (GenericDict comparer root) =
    getHelp comparer targetKey root


getHelp : (k -> k -> Order) -> k -> RBNode k v -> Maybe v
getHelp comparer targetKey root =
    case root of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case comparer targetKey key of
                LT ->
                    getHelp comparer targetKey left

                EQ ->
                    Just value

                GT ->
                    getHelp comparer targetKey right


{-| Determine if a key is in a dictionary.
-}
member : k -> GenericDict k v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : GenericDict k v -> Int
size (GenericDict _ root) =
    sizeHelp 0 root


sizeHelp : Int -> RBNode k v -> Int
sizeHelp n node =
    case node of
        RBEmpty_elm_builtin ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : GenericDict k v -> Bool
isEmpty (GenericDict _ root) =
    case root of
        RBEmpty_elm_builtin ->
            True

        RBNode_elm_builtin _ _ _ _ _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> GenericDict k v -> GenericDict k v
insert key value (GenericDict comparer root) =
    GenericDict comparer
        (-- Root node is always Black
         case insertHelp comparer key value root of
            RBNode_elm_builtin Red k v l r ->
                RBNode_elm_builtin Black k v l r

            x ->
                x
        )


insertHelp : (k -> k -> Order) -> k -> v -> RBNode k v -> RBNode k v
insertHelp comparer key value root =
    case root of
        RBEmpty_elm_builtin ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

        RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
            case comparer key nKey of
                LT ->
                    balance nColor nKey nValue (insertHelp comparer key value nLeft) nRight

                EQ ->
                    RBNode_elm_builtin nColor nKey value nLeft nRight

                GT ->
                    balance nColor nKey nValue nLeft (insertHelp comparer key value nRight)


balance : NColor -> k -> v -> RBNode k v -> RBNode k v -> RBNode k v
balance color key value left right =
    case right of
        RBNode_elm_builtin Red rK rV rLeft rRight ->
            case left of
                RBNode_elm_builtin Red lK lV lLeft lRight ->
                    RBNode_elm_builtin
                        Red
                        key
                        value
                        (RBNode_elm_builtin Black lK lV lLeft lRight)
                        (RBNode_elm_builtin Black rK rV rLeft rRight)

                _ ->
                    RBNode_elm_builtin color rK rV (RBNode_elm_builtin Red key value left rLeft) rRight

        _ ->
            case left of
                RBNode_elm_builtin Red lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight ->
                    RBNode_elm_builtin
                        Red
                        lK
                        lV
                        (RBNode_elm_builtin Black llK llV llLeft llRight)
                        (RBNode_elm_builtin Black key value lRight right)

                _ ->
                    RBNode_elm_builtin color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> GenericDict k v -> GenericDict k v
remove key (GenericDict comparer root) =
    GenericDict comparer
        (-- Root node is always Black
         case removeHelp comparer key root of
            RBNode_elm_builtin Red k v l r ->
                RBNode_elm_builtin Black k v l r

            x ->
                x
        )


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : (k -> k -> Order) -> k -> RBNode k v -> RBNode k v
removeHelp comparer targetKey root =
    case root of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            case comparer targetKey key of
                LT ->
                    case left of
                        RBNode_elm_builtin Black _ _ lLeft _ ->
                            case lLeft of
                                RBNode_elm_builtin Red _ _ _ _ ->
                                    RBNode_elm_builtin color key value (removeHelp comparer targetKey left) right

                                _ ->
                                    case moveRedLeft root of
                                        RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                            balance nColor nKey nValue (removeHelp comparer targetKey nLeft) nRight

                                        RBEmpty_elm_builtin ->
                                            RBEmpty_elm_builtin

                        _ ->
                            RBNode_elm_builtin color key value (removeHelp comparer targetKey left) right

                _ ->
                    removeHelpEQGT comparer targetKey (removeHelpPrepEQGT root color key value left right)


removeHelpPrepEQGT : RBNode k v -> NColor -> k -> v -> RBNode k v -> RBNode k v -> RBNode k v
removeHelpPrepEQGT root color key value left right =
    case left of
        RBNode_elm_builtin Red lK lV lLeft lRight ->
            RBNode_elm_builtin
                color
                lK
                lV
                lLeft
                (RBNode_elm_builtin Red key value lRight right)

        _ ->
            case right of
                RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _ ->
                    moveRedRight root

                RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
                    moveRedRight root

                _ ->
                    root


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : (k -> k -> Order) -> k -> RBNode k v -> RBNode k v
removeHelpEQGT comparer targetKey root =
    case root of
        RBNode_elm_builtin color key value left right ->
            case comparer targetKey key of
                EQ ->
                    case getMin right of
                        RBNode_elm_builtin _ minKey minValue _ _ ->
                            balance color minKey minValue left (removeMin right)

                        RBEmpty_elm_builtin ->
                            RBEmpty_elm_builtin

                _ ->
                    balance color key value left (removeHelp comparer targetKey right)

        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin


getMin : RBNode k v -> RBNode k v
getMin root =
    case root of
        RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            root


removeMin : RBNode k v -> RBNode k v
removeMin root =
    case root of
        RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        RBNode_elm_builtin Red _ _ _ _ ->
                            RBNode_elm_builtin color key value (removeMin left) right

                        _ ->
                            case moveRedLeft root of
                                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeMin nLeft) nRight

                                RBEmpty_elm_builtin ->
                                    RBEmpty_elm_builtin

                _ ->
                    RBNode_elm_builtin color key value (removeMin left) right

        _ ->
            RBEmpty_elm_builtin


moveRedLeft : RBNode k v -> RBNode k v
moveRedLeft root =
    case root of
        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV ((RBNode_elm_builtin Red rlK rlV rlL rlR) as rLeft) rRight) ->
            RBNode_elm_builtin
                Red
                rlK
                rlV
                (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
                (RBNode_elm_builtin Black rK rV rlR rRight)

        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            root


moveRedRight : RBNode k v -> RBNode k v
moveRedRight root =
    case root of
        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            RBNode_elm_builtin
                Red
                lK
                lV
                (RBNode_elm_builtin Black llK llV llLeft llRight)
                (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            root


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> GenericDict k v -> GenericDict k v
update targetKey alter dict =
    case get targetKey dict of
        Nothing ->
            case alter Nothing of
                Nothing ->
                    dict

                Just value ->
                    insert targetKey value dict

        Just oldValue ->
            case alter (Just oldValue) of
                Nothing ->
                    remove targetKey dict

                Just newValue ->
                    insert targetKey newValue dict


{-| Create a dictionary with one key-value pair.
-}
singleton : (k -> k -> Order) -> k -> v -> GenericDict k v
singleton comparer key value =
    -- Root node is always Black
    GenericDict comparer (RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin)



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : GenericDict k v -> GenericDict k v -> GenericDict k v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : GenericDict k v -> GenericDict k v -> GenericDict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : GenericDict k a -> GenericDict k b -> GenericDict k a
diff t1 t2 =
    foldl (\k _ t -> remove k t) t1 t2


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
merge leftStep bothStep rightStep leftDict (GenericDict comparer rightRoot) initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    case comparer lKey rKey of
                        LT ->
                            stepState rKey rValue ( rest, leftStep lKey lValue result )

                        EQ ->
                            ( rest, bothStep lKey lValue rValue result )

                        GT ->
                            ( list, rightStep rKey rValue result )

        ( leftovers, intermediateResult ) =
            foldlHelp stepState ( toList leftDict, initialResult ) rightRoot
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> GenericDict k a -> GenericDict k b
map func (GenericDict comparer root) =
    GenericDict comparer (mapHelp func root)


mapHelp : (k -> a -> b) -> RBNode k a -> RBNode k b
mapHelp func root =
    case root of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            RBNode_elm_builtin color key (func key value) (mapHelp func left) (mapHelp func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages


    -- getAges users == [33,19,28]

-}
foldl : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldl func acc (GenericDict comparer root) =
    foldlHelp func acc root


foldlHelp : (k -> v -> b -> b) -> b -> RBNode k v -> b
foldlHelp func acc root =
    case root of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldlHelp func (func key value (foldlHelp func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages


    -- getAges users == [28,19,33]

-}
foldr : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldr func acc (GenericDict comparer root) =
    foldrHelp func acc root


foldrHelp : (k -> v -> b -> b) -> b -> RBNode k v -> b
foldrHelp func acc root =
    case root of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldrHelp func (func key value (foldrHelp func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> GenericDict k v -> GenericDict k v
filter isGood (GenericDict comparer root) =
    foldlHelp
        (\k v dict ->
            if isGood k v then
                insert k v dict
            else
                dict
        )
        (empty comparer)
        root


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (k -> v -> Bool) -> GenericDict k v -> ( GenericDict k v, GenericDict k v )
partition isGood (GenericDict comparer root) =
    let
        handle key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
    foldlHelp handle ( empty comparer, empty comparer ) root



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : GenericDict k v -> List k
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : GenericDict k v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : GenericDict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : (k -> k -> Order) -> List ( k, v ) -> GenericDict k v
fromList comparer assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) (empty comparer) assocs
