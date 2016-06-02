module Test.GenericSet exposing (tests)

import ElmTest exposing (..)
import GenericSet


compare' : comparable -> comparable -> Order
compare' a a' =
    case compare a a' of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


set : GenericSet.GenericSet Int
set =
    GenericSet.fromList compare [1..100]


setPart1 : GenericSet.GenericSet Int
setPart1 =
    GenericSet.fromList compare [1..50]


setPart2 : GenericSet.GenericSet Int
setPart2 =
    GenericSet.fromList compare [51..100]


pred : Int -> Bool
pred x =
    x <= 50


assertEqualAsStrings : a -> a -> Assertion
assertEqualAsStrings first second =
    assertEqual (toString first) (toString second)


tests : Test
tests =
    let
        orderingTests =
            suite "ordering Tests"
                [ test "orders by the comparer"
                    <| assertEqual (List.reverse [1..100])
                        (GenericSet.toList <| GenericSet.fromList compare' [1..100])
                ]

        modifierTests =
            suite "modifier Tests"
                [ test "insert adds a value"
                    <| assertEqual [ 1, 2, 3, 5 ]
                        (GenericSet.fromList compare [1..3]
                            |> GenericSet.insert 5
                            |> GenericSet.toList
                        )
                , test "insert does nothing if a value exists"
                    <| assertEqual [ 1, 2, 3 ]
                        (GenericSet.fromList compare [1..3]
                            |> GenericSet.insert 2
                            |> GenericSet.toList
                        )
                , test "remove deletes a value"
                    <| assertEqual [ 1, 3 ]
                        (GenericSet.fromList compare [1..3]
                            |> GenericSet.remove 2
                            |> GenericSet.toList
                        )
                , test "remove does nothing if a value does not exist"
                    <| assertEqual [ 1, 2, 3 ]
                        (GenericSet.fromList compare [1..3]
                            |> GenericSet.remove 5
                            |> GenericSet.toList
                        )
                ]

        queryTests =
            suite "query Tests"
                [ test "isEmpty returns True for an empty set"
                    <| assertEqual True (GenericSet.isEmpty <| GenericSet.empty compare)
                , test "isEmpty returns False for a non-empty set"
                    <| assertEqual False (GenericSet.isEmpty set)
                , test "member returns True if set includes value"
                    <| assertEqual True (GenericSet.member 50 set)
                , test "member returns False if set does not include value"
                    <| assertEqual False (GenericSet.member 0 set)
                , test "member uses the comparer to determine equality"
                    <| let
                        comparer a a' =
                            compare a.id a'.id

                        set' =
                            GenericSet.singleton comparer { id = 1, name = "thing" }
                       in
                        assertEqual True (GenericSet.member { id = 1, name = "other" } set')
                , test "size of set of 100 elements"
                    <| assertEqual 100 (GenericSet.size set)
                , test "size of an empty set"
                    <| assertEqual 0 (GenericSet.size <| GenericSet.empty compare)
                ]

        combiningTests =
            suite "combining Tests"
                [ test "union combines two sets"
                    <| assertEqualAsStrings set (GenericSet.union setPart1 setPart2)
                , test "union uses the comparer from the first set"
                    <| let
                        set1 =
                            GenericSet.fromList compare' [ 3, 2, 1 ]

                        set2 =
                            GenericSet.fromList compare [ 3, 4, 5 ]
                       in
                        assertEqual [ 5, 4, 3, 2, 1 ] (GenericSet.union set1 set2 |> GenericSet.toList)
                , test "intersect gets the overlap of two sets"
                    <| let
                        set1 =
                            GenericSet.fromList compare [ 1, 2, 3 ]

                        set2 =
                            GenericSet.fromList compare [ 2, 3, 4 ]
                       in
                        assertEqual [ 2, 3 ] (GenericSet.intersect set1 set2 |> GenericSet.toList)
                , test "intersect uses the comparer from the first set"
                    <| let
                        set1 =
                            GenericSet.fromList compare' [ 3, 2, 1 ]

                        set2 =
                            GenericSet.fromList compare [ 2, 3, 4 ]
                       in
                        assertEqual [ 3, 2 ] (GenericSet.intersect set1 set2 |> GenericSet.toList)
                , test "diff gets the values in the first set that are not in the second"
                    <| let
                        set1 =
                            GenericSet.fromList compare [ 1, 2, 3, 4 ]

                        set2 =
                            GenericSet.fromList compare' [ 3, 4, 5 ]
                       in
                        assertEqual [ 1, 2 ] (GenericSet.diff set1 set2 |> GenericSet.toList)
                ]

        transformTests =
            suite "transform Tests"
                [ test "Simple map"
                    <| assertEqualAsStrings setPart1 (GenericSet.map compare (\n -> n - 50) setPart2)
                , test "Simple foldl"
                    <| let
                        set' =
                            GenericSet.fromList compare [ 1, 2, 3 ]
                       in
                        assertEqual "321" (GenericSet.foldl (toString >> (++)) "" set')
                , test "Simple foldr"
                    <| let
                        set' =
                            GenericSet.fromList compare [ 1, 2, 3 ]
                       in
                        assertEqual "123" (GenericSet.foldr (toString >> (++)) "" set')
                , test "Simple filter"
                    <| assertEqualAsStrings setPart1 (GenericSet.filter pred set)
                , test "Simple partition"
                    <| assertEqualAsStrings ( setPart1, setPart2 ) (GenericSet.partition pred set)
                ]
    in
        suite "GenericSet Tests"
            [ orderingTests
            , modifierTests
            , queryTests
            , combiningTests
            , transformTests
            ]
