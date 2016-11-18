module Array2D.ArrayHelpers exposing (..)

{-| Internal helpers for manipulating Arrays of Arrays.
-}

import Array exposing (Array)


{-| Delete an element from an array. Does nothing if index is out of bounds.
-}
deleteArrayElt : Int -> Array a -> Array a
deleteArrayElt index array =
    let
        lastIndex =
            (Array.length array) - 1

        first =
            Array.slice 0 index array

        last =
            Array.slice (index + 1) (Array.length array) array
    in
        if (index > lastIndex) || (index < 0) then
            array
        else
            Array.append first last


{-| Find the length of the shortest sub-array in an array of arrays.
-}
minRowLength : Array (Array a) -> Int
minRowLength array =
    if Array.isEmpty array then
        0
    else
        Array.foldl
            (\row min ->
                let
                    rowLen =
                        Array.length row
                in
                    if min == -1 then
                        rowLen
                    else if rowLen < min then
                        rowLen
                    else
                        min
            )
            -1
            array


{-| Truncate all sub-arrays down to a maximum length. Does
nothing if they are too short!
-}
truncateRows : Int -> Array (Array a) -> Array (Array a)
truncateRows columns array =
    array
        |> Array.map
            (\row ->
                if (Array.length row) > columns then
                    Array.slice 0 columns row
                else
                    row
            )


{-| Combine both the above operations
-}
getMinColumnsAndTruncateRows : Array (Array a) -> ( Int, Array (Array a) )
getMinColumnsAndTruncateRows array =
    let
        columns =
            minRowLength array

        normalizedData =
            truncateRows columns array
    in
        ( columns, normalizedData )


{-| Normalize an Array to a specific length, using filler if it is too short,
   or truncating if too long.
-}
normalize : Int -> a -> Array a -> Array a
normalize length filler input =
    let
        inputLength =
            Array.length input
    in
        if inputLength > length then
            Array.slice 0 length input
        else if inputLength < length then
            Array.append input (Array.repeat (length - inputLength) filler)
        else
            input
