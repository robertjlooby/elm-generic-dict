module GenericDict exposing (GenericDict, empty, singleton, insert, update, isEmpty, get, remove, member, size, filter, partition, foldl, foldr, map, union, intersect, diff, keys, values, toList, fromList)

{-| A dictionary mapping unique keys to values. The keys can be any type. The
builder functions take a comparer function that takes two keys and returns an
Order.

Insert, remove, and query operations all take *O(log n)* time. Dictionary
equality with `(==)` is unreliable and should not be used.

# Dictionaries
@docs GenericDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

import Debug
import Dict as CoreDict
import String


type NColor
    = Red
    | Black
    | BBlack
    | NBlack


type LeafColor
    = LBlack
    | LBBlack


{-| A dictionary of keys and values. So a `(GenericDict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type GenericDict k v
    = RBNode_elm_builtin NColor k v (GenericDict k v) (GenericDict k v) (k -> k -> Order)
    | RBEmpty_elm_builtin LeafColor (k -> k -> Order)


getComparer : GenericDict k v -> (k -> k -> Order)
getComparer dict =
    case dict of
        RBNode_elm_builtin _ _ _ _ _ comparer ->
            comparer

        RBEmpty_elm_builtin _ comparer ->
            comparer


{-| Create an empty dictionary using the given comparer.
-}
empty : (k -> k -> Order) -> GenericDict k v
empty comparer =
    RBEmpty_elm_builtin LBlack comparer


maxWithDefault : k -> v -> GenericDict k v -> ( k, v )
maxWithDefault k v r =
    case r of
        RBEmpty_elm_builtin _ _ ->
            ( k, v )

        RBNode_elm_builtin _ kr vr _ rr _ ->
            maxWithDefault kr vr rr


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList compare [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> GenericDict k v -> Maybe v
get targetKey dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            Nothing

        RBNode_elm_builtin _ key value left right comparer ->
            case comparer targetKey key of
                LT ->
                    get targetKey left

                EQ ->
                    Just value

                GT ->
                    get targetKey right


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
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> GenericDict k v -> Int
sizeHelp n dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            n

        RBNode_elm_builtin _ _ _ left right _ ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Determine if a dictionary is empty.

    isEmpty (empty compare) == True
-}
isEmpty : GenericDict k v -> Bool
isEmpty dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            True

        RBNode_elm_builtin _ _ _ _ _ _ ->
            False



{- The actual pattern match here is somewhat lax. If it is given invalid input,
   it will do the wrong thing. The expected behavior is:

     red node => black node
     black node => same
     bblack node => xxx
     nblack node => xxx

     black leaf => same
     bblack leaf => xxx
-}


ensureBlackRoot : GenericDict k v -> GenericDict k v
ensureBlackRoot dict =
    case dict of
        RBNode_elm_builtin Red key value left right comparer ->
            RBNode_elm_builtin Black key value left right comparer

        _ ->
            dict


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> GenericDict k v -> GenericDict k v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> GenericDict k v -> GenericDict k v
remove key dict =
    update key (always Nothing) dict


type Flag
    = Insert
    | Remove
    | Same


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> GenericDict k v -> GenericDict k v
update k alter dict =
    let
        up dict =
            case dict of
                -- expecting only black nodes, never double black nodes here
                RBEmpty_elm_builtin _ comparer ->
                    case alter Nothing of
                        Nothing ->
                            ( Same, empty comparer )

                        Just v ->
                            ( Insert, RBNode_elm_builtin Red k v (empty comparer) (empty comparer) comparer )

                RBNode_elm_builtin clr key value left right comparer ->
                    case comparer k key of
                        EQ ->
                            case alter (Just value) of
                                Nothing ->
                                    ( Remove, rem clr left right )

                                Just newValue ->
                                    ( Same, RBNode_elm_builtin clr key newValue left right comparer )

                        LT ->
                            let
                                ( flag, newLeft ) =
                                    up left
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value newLeft right comparer )

                                    Insert ->
                                        ( Insert, balance clr key value newLeft right )

                                    Remove ->
                                        ( Remove, bubble clr key value newLeft right )

                        GT ->
                            let
                                ( flag, newRight ) =
                                    up right
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value left newRight comparer )

                                    Insert ->
                                        ( Insert, balance clr key value left newRight )

                                    Remove ->
                                        ( Remove, bubble clr key value left newRight )

        ( flag, updatedDict ) =
            up dict
    in
        case flag of
            Same ->
                updatedDict

            Insert ->
                ensureBlackRoot updatedDict

            Remove ->
                blacken updatedDict


{-| Create a dictionary with one key-value pair, using the given comparer.
-}
singleton : (k -> k -> Order) -> k -> v -> GenericDict k v
singleton comparer key value =
    insert key value <| empty comparer


isBBlack : GenericDict k v -> Bool
isBBlack dict =
    case dict of
        RBNode_elm_builtin BBlack _ _ _ _ _ ->
            True

        RBEmpty_elm_builtin LBBlack _ ->
            True

        _ ->
            False


moreBlack : NColor -> NColor
moreBlack color =
    case color of
        Black ->
            BBlack

        Red ->
            Black

        NBlack ->
            Red

        BBlack ->
            Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
    case color of
        BBlack ->
            Black

        Black ->
            Red

        Red ->
            NBlack

        NBlack ->
            Debug.crash "Can't make a negative black node less black!"



{- The actual pattern match here is somewhat lax. If it is given invalid input,
   it will do the wrong thing. The expected behavior is:

     node => less black node

     bblack leaf => black leaf
     black leaf => xxx
-}


lessBlackTree : GenericDict k v -> GenericDict k v
lessBlackTree dict =
    case dict of
        RBNode_elm_builtin c k v l r comparer ->
            RBNode_elm_builtin (lessBlack c) k v l r comparer

        RBEmpty_elm_builtin _ comparer ->
            RBEmpty_elm_builtin LBlack comparer


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
    Debug.crash
        <| String.concat
            [ "Internal red-black tree invariant violated, expected "
            , msg
            , " and got "
            , toString c
            , "/"
            , lgot
            , "/"
            , rgot
            , "\nPlease report this bug to <https://github.com/elm-lang/core/issues>"
            ]


rem : NColor -> GenericDict k v -> GenericDict k v -> GenericDict k v
rem c l r =
    case ( l, r ) of
        ( RBEmpty_elm_builtin _ comparer, RBEmpty_elm_builtin _ _ ) ->
            case c of
                Red ->
                    RBEmpty_elm_builtin LBlack comparer

                Black ->
                    RBEmpty_elm_builtin LBBlack comparer

                _ ->
                    Debug.crash "cannot have bblack or nblack nodes at this point"

        ( RBEmpty_elm_builtin cl comparer, RBNode_elm_builtin cr key value left right _ ) ->
            case ( c, cl, cr ) of
                ( Black, LBlack, Red ) ->
                    RBNode_elm_builtin Black key value left right comparer

                _ ->
                    reportRemBug "Black/LBlack/Red" c (toString cl) (toString cr)

        ( RBNode_elm_builtin cl key value left right comparer, RBEmpty_elm_builtin cr _ ) ->
            case ( c, cl, cr ) of
                ( Black, Red, LBlack ) ->
                    RBNode_elm_builtin Black key value left right comparer

                _ ->
                    reportRemBug "Black/Red/LBlack" c (toString cl) (toString cr)

        ( RBNode_elm_builtin cl kl vl ll rl comparer, RBNode_elm_builtin _ _ _ _ _ _ ) ->
            let
                ( k, v ) =
                    maxWithDefault kl vl rl

                left =
                    removeMax cl kl vl ll rl
            in
                bubble c k v left r


bubble : NColor -> k -> v -> GenericDict k v -> GenericDict k v -> GenericDict k v
bubble c k v l r =
    if isBBlack l || isBBlack r then
        balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
    else
        RBNode_elm_builtin c k v l r <| getComparer l


removeMax : NColor -> k -> v -> GenericDict k v -> GenericDict k v -> GenericDict k v
removeMax c k v l r =
    case r of
        RBEmpty_elm_builtin _ _ ->
            rem c l r

        RBNode_elm_builtin cr kr vr lr rr _ ->
            bubble c k v l (removeMax cr kr vr lr rr)


balance : NColor -> k -> v -> GenericDict k v -> GenericDict k v -> GenericDict k v
balance c k v l r =
    let
        tree =
            RBNode_elm_builtin c k v l r <| getComparer l
    in
        if blackish tree then
            balanceHelp tree
        else
            tree


blackish : GenericDict k v -> Bool
blackish t =
    case t of
        RBNode_elm_builtin c _ _ _ _ _ ->
            c == Black || c == BBlack

        RBEmpty_elm_builtin _ _ ->
            True


balanceHelp : GenericDict k v -> GenericDict k v
balanceHelp tree =
    case tree of
        RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red yk yv (RBNode_elm_builtin Red xk xv a b _) c _) d comparer ->
            balancedTree col xk xv yk yv zk zv a b c d comparer

        RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red xk xv a (RBNode_elm_builtin Red yk yv b c _) _) d comparer ->
            balancedTree col xk xv yk yv zk zv a b c d comparer

        RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red zk zv (RBNode_elm_builtin Red yk yv b c _) d _) comparer ->
            balancedTree col xk xv yk yv zk zv a b c d comparer

        RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red yk yv b (RBNode_elm_builtin Red zk zv c d _) _) comparer ->
            balancedTree col xk xv yk yv zk zv a b c d comparer

        RBNode_elm_builtin BBlack xk xv a (RBNode_elm_builtin NBlack zk zv (RBNode_elm_builtin Black yk yv b c _) ((RBNode_elm_builtin Black _ _ _ _ _) as d) _) comparer ->
            RBNode_elm_builtin Black yk yv (RBNode_elm_builtin Black xk xv a b comparer) (balance Black zk zv c (redden d)) comparer

        RBNode_elm_builtin BBlack zk zv (RBNode_elm_builtin NBlack xk xv ((RBNode_elm_builtin Black _ _ _ _ _) as a) (RBNode_elm_builtin Black yk yv b c _) _) d comparer ->
            RBNode_elm_builtin Black yk yv (balance Black xk xv (redden a) b) (RBNode_elm_builtin Black zk zv c d comparer) comparer

        _ ->
            tree


balancedTree : NColor -> k -> v -> k -> v -> k -> v -> GenericDict k v -> GenericDict k v -> GenericDict k v -> GenericDict k v -> (k -> k -> Order) -> GenericDict k v
balancedTree col xk xv yk yv zk zv a b c d comparer =
    RBNode_elm_builtin (lessBlack col)
        yk
        yv
        (RBNode_elm_builtin Black xk xv a b comparer)
        (RBNode_elm_builtin Black zk zv c d comparer)
        comparer


blacken : GenericDict k v -> GenericDict k v
blacken t =
    case t of
        RBEmpty_elm_builtin _ comparer ->
            RBEmpty_elm_builtin LBlack comparer

        RBNode_elm_builtin _ k v l r comparer ->
            RBNode_elm_builtin Black k v l r comparer


redden : GenericDict k v -> GenericDict k v
redden t =
    case t of
        RBEmpty_elm_builtin _ _ ->
            Debug.crash "can't make a Leaf red"

        RBNode_elm_builtin _ k v l r comparer ->
            RBNode_elm_builtin Red k v l r comparer


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> GenericDict k a -> GenericDict k b
map f dict =
    case dict of
        RBEmpty_elm_builtin _ comparer ->
            RBEmpty_elm_builtin LBlack comparer

        RBNode_elm_builtin clr key value left right comparer ->
            RBNode_elm_builtin clr key (f key value) (map f left) (map f right) comparer


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key (by the comparer).
-}
foldl : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldl f acc dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            acc

        RBNode_elm_builtin _ key value left right _ ->
            foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key (by the comparer).
-}
foldr : (k -> v -> b -> b) -> b -> GenericDict k v -> b
foldr f acc t =
    case t of
        RBEmpty_elm_builtin _ _ ->
            acc

        RBNode_elm_builtin _ key value left right _ ->
            foldr f (f key value (foldr f acc right)) left


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. Keep the comparer from the first dictionary.
-}
union : GenericDict k v -> GenericDict k v -> GenericDict k v
union t1 t2 =
    let
        comparer =
            getComparer t1

        reOrderedT2 =
            foldl insert (empty comparer) t2
    in
        foldl insert reOrderedT2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. Keep the comparer from
the first dictionary.
-}
intersect : GenericDict k v -> GenericDict k v -> GenericDict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second
dictionary. Keep the comparer from the first dictionary.
-}
diff : GenericDict k v -> GenericDict k v -> GenericDict k v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : GenericDict k v -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : GenericDict k v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : GenericDict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary using the given comparer.
-}
fromList : (k -> k -> Order) -> List ( k, v ) -> GenericDict k v
fromList comparer assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) (empty comparer) assocs


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> GenericDict k v -> GenericDict k v
filter predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        foldl add (empty <| getComparer dictionary) dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> GenericDict k v -> ( GenericDict k v, GenericDict k v )
partition predicate dict =
    let
        comparer =
            getComparer dict

        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
        foldl add ( empty comparer, empty comparer ) dict
