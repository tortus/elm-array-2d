module Array2DTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Array2D
import Array


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
                        |> Array.toList
                        |> List.map (Maybe.withDefault -1)
                        |> Expect.equal [ 0, 1, 2 ]
            , test "calls f with the correct column" <|
                \_ ->
                    Array2D.initialize 2 3 (\_ col -> col)
                        |> Array2D.getRow 0
                        |> Maybe.map Array.toList
                        |> Maybe.withDefault []
                        |> Expect.equal [ 0, 1, 2 ]
            ]
        ]
