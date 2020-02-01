module Array2DTest exposing (suite)

import Array
import Array2D
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Array2D module"
        [ describe "Array2D.initialize rows cols f"
            [ test "creates the correct number of rows" <|
                \_ ->
                    Array2D.initialize 2 3 (\row col -> row + col)
                        |> Array2D.rows
                        |> Expect.equal 2
            , test "creates the correct number of columns" <|
                \_ ->
                    Array2D.initialize 2 3 (\row col -> row + col)
                        |> Array2D.columns
                        |> Expect.equal 3
            , test "calls f with the correct row" <|
                \_ ->
                    Array2D.initialize 3 2 (\row _ -> row)
                        |> Array2D.getColumn 0
                        |> Expect.equal (Just (Array.fromList [ 0, 1, 2 ]))
            , test "calls f with the correct column" <|
                \_ ->
                    Array2D.initialize 2 3 (\_ col -> col)
                        |> Array2D.getRow 0
                        |> Maybe.map Array.toList
                        |> Maybe.withDefault []
                        |> Expect.equal [ 0, 1, 2 ]
            , test "getColumn example is correct" <|
                \_ ->
                    Array2D.getColumn 1 (Array2D.fromList [ [ 1, 2 ], [ 3, 4 ] ])
                        |> Expect.equal (Just (Array.fromList [ 2, 4 ]))
            , test "getColumn has correct behavior for Array2d with no rows" <|
                \_ ->
                    Array2D.initialize 0 3 (\row _ -> row)
                        |> Array2D.getColumn 1
                        |> Expect.equal (Just Array.empty)
            , test "getColumn has correct behavior for Array2d with no columns" <|
                \_ ->
                    Array2D.initialize 3 0 (\row _ -> row)
                        |> Array2D.getColumn 1
                        |> Expect.equal Nothing
            ]
        ]
