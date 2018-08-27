module GenericSetTest exposing (tests)

import Expect exposing (Expectation)
import GenericSet
import Test exposing (Test, describe, skip, test)


compare2 : comparable -> comparable -> Order
compare2 a b =
    case compare a b of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


set : GenericSet.GenericSet Int
set =
    GenericSet.fromList compare (List.range 1 100)


setPart1 : GenericSet.GenericSet Int
setPart1 =
    GenericSet.fromList compare (List.range 1 50)


setPart2 : GenericSet.GenericSet Int
setPart2 =
    GenericSet.fromList compare (List.range 51 100)


pred : Int -> Bool
pred x =
    x <= 50


assertEqualAsStrings : a -> a -> Expectation
assertEqualAsStrings first second =
    Expect.equal (Debug.toString first) (Debug.toString second)


tests : Test
tests =
    let
        orderingTests =
            describe "ordering Tests"
                [ test "orders by the comparer" <|
                    \() ->
                        GenericSet.fromList compare2 (List.range 1 100)
                            |> GenericSet.toList
                            |> Expect.equal (List.reverse (List.range 1 100))
                ]

        modifierTests =
            describe "modifier Tests"
                [ test "insert adds a value" <|
                    \() ->
                        GenericSet.fromList compare (List.range 1 3)
                            |> GenericSet.insert 5
                            |> GenericSet.toList
                            |> Expect.equal [ 1, 2, 3, 5 ]
                , test "insert does nothing if a value exists" <|
                    \() ->
                        GenericSet.fromList compare (List.range 1 3)
                            |> GenericSet.insert 2
                            |> GenericSet.toList
                            |> Expect.equal [ 1, 2, 3 ]
                , test "remove deletes a value" <|
                    \() ->
                        GenericSet.fromList compare (List.range 1 3)
                            |> GenericSet.remove 2
                            |> GenericSet.toList
                            |> Expect.equal [ 1, 3 ]
                , test "remove does nothing if a value does not exist" <|
                    \() ->
                        GenericSet.fromList compare (List.range 1 3)
                            |> GenericSet.remove 5
                            |> GenericSet.toList
                            |> Expect.equal [ 1, 2, 3 ]
                ]

        queryTests =
            describe "query Tests"
                [ test "isEmpty returns True for an empty set" <|
                    \() ->
                        GenericSet.empty compare
                            |> GenericSet.isEmpty
                            |> Expect.equal True
                , test "isEmpty returns False for a non-empty set" <|
                    \() ->
                        GenericSet.isEmpty set
                            |> Expect.equal False
                , test "member returns True if set includes value" <|
                    \() ->
                        GenericSet.member 50 set
                            |> Expect.equal True
                , test "member returns False if set does not include value" <|
                    \() ->
                        GenericSet.member 0 set
                            |> Expect.equal False
                , test "member uses the comparer to determine equality" <|
                    \() ->
                        let
                            comparer a b =
                                compare a.id b.id

                            set2 =
                                GenericSet.singleton comparer { id = 1, name = "thing" }
                        in
                        GenericSet.member { id = 1, name = "other" } set2
                            |> Expect.equal True
                , test "size of set of 100 elements" <|
                    \() ->
                        GenericSet.size set
                            |> Expect.equal 100
                , test "size of an empty set" <|
                    \() ->
                        GenericSet.empty compare
                            |> GenericSet.size
                            |> Expect.equal 0
                ]

        combiningTests =
            describe "combining Tests"
                [ test "union combines two sets" <|
                    \() ->
                        assertEqualAsStrings set (GenericSet.union setPart1 setPart2)
                , skip <|
                    test "union uses the comparer from the first set" <|
                        \() ->
                            let
                                set1 =
                                    GenericSet.fromList compare2 [ 3, 2, 1 ]

                                set2 =
                                    GenericSet.fromList compare [ 3, 4, 5 ]
                            in
                            GenericSet.union set1 set2
                                |> GenericSet.toList
                                |> Expect.equal [ 5, 4, 3, 2, 1 ]
                , test "intersect gets the overlap of two sets" <|
                    \() ->
                        let
                            set1 =
                                GenericSet.fromList compare [ 1, 2, 3 ]

                            set2 =
                                GenericSet.fromList compare [ 2, 3, 4 ]
                        in
                        GenericSet.intersect set1 set2
                            |> GenericSet.toList
                            |> Expect.equal [ 2, 3 ]
                , skip <|
                    test "intersect uses the comparer from the first set" <|
                        \() ->
                            let
                                set1 =
                                    GenericSet.fromList compare2 [ 3, 2, 1 ]

                                set2 =
                                    GenericSet.fromList compare [ 2, 3, 4 ]
                            in
                            GenericSet.intersect set1 set2
                                |> GenericSet.toList
                                |> Expect.equal [ 3, 2 ]
                , skip <|
                    test "diff gets the values in the first set that are not in the second" <|
                        \() ->
                            let
                                set1 =
                                    GenericSet.fromList compare [ 1, 2, 3, 4 ]

                                set2 =
                                    GenericSet.fromList compare2 [ 3, 4, 5 ]
                            in
                            GenericSet.diff set1 set2
                                |> GenericSet.toList
                                |> Expect.equal [ 1, 2 ]
                ]

        transformTests =
            describe "transform Tests"
                [ test "Simple map" <|
                    \() ->
                        GenericSet.map compare (\n -> n - 50) setPart2
                            |> assertEqualAsStrings setPart1
                , test "Simple foldl" <|
                    \() ->
                        let
                            set2 =
                                GenericSet.fromList compare [ 1, 2, 3 ]
                        in
                        GenericSet.foldl (Debug.toString >> (++)) "" set2
                            |> Expect.equal "321"
                , test "Simple foldr" <|
                    \() ->
                        let
                            set2 =
                                GenericSet.fromList compare [ 1, 2, 3 ]
                        in
                        GenericSet.foldr (Debug.toString >> (++)) "" set2
                            |> Expect.equal "123"
                , test "Simple filter" <|
                    \() ->
                        GenericSet.filter pred set
                            |> assertEqualAsStrings setPart1
                , test "Simple partition" <|
                    \() ->
                        GenericSet.partition pred set
                            |> assertEqualAsStrings ( setPart1, setPart2 )
                ]
    in
    describe "GenericSet Tests"
        [ orderingTests
        , modifierTests
        , queryTests
        , combiningTests
        , transformTests
        ]
