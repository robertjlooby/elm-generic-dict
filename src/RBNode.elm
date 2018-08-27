module RBNode
    exposing
        ( RBNode
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

-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


type RBNode k v
    = RBNode_elm_builtin NColor k v (RBNode k v) (RBNode k v)
    | RBEmpty_elm_builtin


empty : RBNode k v
empty =
    RBEmpty_elm_builtin


get : (k -> k -> Order) -> k -> RBNode k v -> Maybe v
get comparer targetKey node =
    case node of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case comparer targetKey key of
                LT ->
                    get comparer targetKey left

                EQ ->
                    Just value

                GT ->
                    get comparer targetKey right


member : (k -> k -> Order) -> k -> RBNode k v -> Bool
member comparer key node =
    case get comparer key node of
        Just _ ->
            True

        Nothing ->
            False


size : RBNode k v -> Int
size node =
    sizeHelp 0 node


sizeHelp : Int -> RBNode k v -> Int
sizeHelp n node =
    case node of
        RBEmpty_elm_builtin ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


isEmpty : RBNode k v -> Bool
isEmpty node =
    case node of
        RBEmpty_elm_builtin ->
            True

        RBNode_elm_builtin _ _ _ _ _ ->
            False


insert : (k -> k -> Order) -> k -> v -> RBNode k v -> RBNode k v
insert comparer key value node =
    -- Root node is always Black
    case insertHelp comparer key value node of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


insertHelp : (k -> k -> Order) -> k -> v -> RBNode k v -> RBNode k v
insertHelp comparer key value node =
    case node of
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


remove : (k -> k -> Order) -> k -> RBNode k v -> RBNode k v
remove comparer key node =
    -- Root node is always Black
    case removeHelp comparer key node of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : (k -> k -> Order) -> k -> RBNode k v -> RBNode k v
removeHelp comparer targetKey node =
    case node of
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
                                    case moveRedLeft node of
                                        RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                            balance nColor nKey nValue (removeHelp comparer targetKey nLeft) nRight

                                        RBEmpty_elm_builtin ->
                                            RBEmpty_elm_builtin

                        _ ->
                            RBNode_elm_builtin color key value (removeHelp comparer targetKey left) right

                _ ->
                    removeHelpEQGT comparer targetKey (removeHelpPrepEQGT node color key value left right)


removeHelpPrepEQGT : RBNode k v -> NColor -> k -> v -> RBNode k v -> RBNode k v -> RBNode k v
removeHelpPrepEQGT node color key value left right =
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
                    moveRedRight node

                RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
                    moveRedRight node

                _ ->
                    node


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : (k -> k -> Order) -> k -> RBNode k v -> RBNode k v
removeHelpEQGT comparer targetKey node =
    case node of
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
getMin node =
    case node of
        RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            node


removeMin : RBNode k v -> RBNode k v
removeMin node =
    case node of
        RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        RBNode_elm_builtin Red _ _ _ _ ->
                            RBNode_elm_builtin color key value (removeMin left) right

                        _ ->
                            case moveRedLeft node of
                                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeMin nLeft) nRight

                                RBEmpty_elm_builtin ->
                                    RBEmpty_elm_builtin

                _ ->
                    RBNode_elm_builtin color key value (removeMin left) right

        _ ->
            RBEmpty_elm_builtin


moveRedLeft : RBNode k v -> RBNode k v
moveRedLeft node =
    case node of
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
            node


moveRedRight : RBNode k v -> RBNode k v
moveRedRight node =
    case node of
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
            node


update : (k -> k -> Order) -> k -> (Maybe v -> Maybe v) -> RBNode k v -> RBNode k v
update comparer targetKey alter node =
    case get comparer targetKey node of
        Nothing ->
            case alter Nothing of
                Nothing ->
                    node

                Just value ->
                    insert comparer targetKey value node

        Just oldValue ->
            case alter (Just oldValue) of
                Nothing ->
                    remove comparer targetKey node

                Just newValue ->
                    insert comparer targetKey newValue node


singleton : k -> v -> RBNode k v
singleton key value =
    -- Root node is always Black
    RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin



-- COMBINE


union : (k -> k -> Order) -> RBNode k v -> RBNode k v -> RBNode k v
union comparer t1 t2 =
    foldl (insert comparer) t2 t1


intersect : (k -> k -> Order) -> RBNode k v -> RBNode k v -> RBNode k v
intersect comparer t1 t2 =
    filter comparer (\k _ -> member comparer k t2) t1


diff : (k -> k -> Order) -> RBNode k a -> RBNode k b -> RBNode k a
diff comparer t1 t2 =
    foldl (\k v t -> remove comparer k t) t1 t2


merge :
    (k -> k -> Order)
    -> (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> RBNode k a
    -> RBNode k b
    -> result
    -> result
merge comparer leftStep bothStep rightStep leftNode rightNode initialResult =
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
            foldl stepState ( toList leftNode, initialResult ) rightNode
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


map : (k -> a -> b) -> RBNode k a -> RBNode k b
map func node =
    case node of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            RBNode_elm_builtin color key (func key value) (map func left) (map func right)


foldl : (k -> v -> b -> b) -> b -> RBNode k v -> b
foldl func acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl func (func key value (foldl func acc left)) right


foldr : (k -> v -> b -> b) -> b -> RBNode k v -> b
foldr func acc node =
    case node of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr func (func key value (foldr func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> k -> Order) -> (k -> v -> Bool) -> RBNode k v -> RBNode k v
filter comparer isGood node =
    foldl
        (\k v acc ->
            if isGood k v then
                insert comparer k v acc
            else
                acc
        )
        empty
        node


partition : (k -> k -> Order) -> (k -> v -> Bool) -> RBNode k v -> ( RBNode k v, RBNode k v )
partition comparer isGood node =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert comparer key value t1, t2 )
            else
                ( t1, insert comparer key value t2 )
    in
    foldl add ( empty, empty ) node



-- LISTS


keys : RBNode k v -> List k
keys node =
    foldr (\key value keyList -> key :: keyList) [] node


values : RBNode k v -> List v
values node =
    foldr (\key value valueList -> value :: valueList) [] node


toList : RBNode k v -> List ( k, v )
toList node =
    foldr (\key value list -> ( key, value ) :: list) [] node


fromList : (k -> k -> Order) -> List ( k, v ) -> RBNode k v
fromList comparer assocs =
    List.foldl (\( key, value ) node -> insert comparer key value node) empty assocs
