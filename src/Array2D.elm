module Array2D
    exposing
        ( Array2D
        , empty
        , fromArray
        , fromList
        , initialize
        , repeat
        , rows
        , columns
        , isEmpty
        , get
        , set
        , getRow
        , appendRow
        , deleteRow
        , getColumn
        , appendColumn
        , deleteColumn
        , map
        , indexedMap
        )

{-| Implements 2D array using nested Arrays. Useful for implementing data grids,
as it specifically provides row and column operations.

Cells are accessed by row, then column. Otherwise, it works very similarly to the
Array class. The documentation examples usually omit converting Lists to Arrays
for brevity.

Array2D's can be constructed from an Array or List of rows, where each
row is an Array or List of cells.

    Array2D.fromList
      [ ["Row 1-Col 1", "Row 1-Col 2"]
      , ["Row 2-Col 1", "Row 2-Col 2"]
      ]

If the nested arrays happen to be jagged, all rows will be truncated to the
length of the smallest row! Be careful!

    Array2D.fromList
      [ [0, 1]
      , [0, 1, 2]
      ]
    -- Becomes:
      [ [0, 1]
      , [0, 1]
      ]


## Drawbacks and caveats

Most examples of nested models in Elm use Lists of elements with a
unique, constant ID, e.g.:

    type alias Cell = { uid : Int, ... }

This allows messages to always be routed to the correct
element, even if elements are re-ordered, removed, added, etc.
If you use the **index** of an element instead to create a long Task
that will change the element when it ends, be aware that the target
element's index may have changed during the task!

For data grids you are probably not going to be re-positioning
cells. Most data grids simply modify cells in place, which is what
Array2D is mainly intended for. **The danger comes from inserting and
deleting rows and columns.** During such operations, you may want to
temporarily make your grid "read-only" somehow.


# Base type
@docs Array2D

# Initialization
@docs empty, fromArray, fromList, initialize, repeat

# Getting info
@docs rows, columns, isEmpty

# Fetching/updating individual cells
@docs get, set

# Adding/removing rows
@docs getRow, appendRow, deleteRow

# Adding/removing columns
@docs getColumn, appendColumn, deleteColumn

# Mapping cell data
@docs map, indexedMap
-}

import Array exposing (Array)
import Array2D.ArrayHelpers as Helpers


{-| Base Array2D type
-}
type alias Array2D a =
    { data : Array (Array a)
    , columns : Int
    }


{-| Create an empty Array2D
-}
empty : Array2D a
empty =
    { data = Array.empty
    , columns = 0
    }


{-| Create an Array2D from an Array of Arrays. All rows will
be truncated to the length of the shortest row.

    let
      row1 = Array.fromList [1, 2]
      row2 = Array.fromList [2, 3]
    in
      fromArray (Array.fromList [row1, row2])
-}
fromArray : Array (Array a) -> Array2D a
fromArray array =
    let
        ( columns, normalizedData ) =
            Helpers.getMinColumnsAndTruncateRows array
    in
        { data = normalizedData
        , columns = columns
        }


{-| Create an Array2D from a List of Lists.

    fromList [[1, 2, 3], [4, 5, 6]]
-}
fromList : List (List a) -> Array2D a
fromList list =
    list
        |> List.map Array.fromList
        |> Array.fromList
        |> fromArray


{-| Initialize an Array2D. `initialize rows cols f` creates an array
with the given dimensions with the element at index `row col`
initialized to the result of `(f row col)`. Similar to
`Array.initialize`.

    initialize 2 3 (\row col -> row + col)  == fromList [[0, 1, 2], [1, 2, 3]]
-}
initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize numRows numColumns f =
    let
        rows =
            Array.initialize
                numRows
                (\row ->
                    (Array.initialize
                        numColumns
                        (\col -> f row col)
                    )
                )
    in
        { data = rows
        , columns = numColumns
        }


{-| Create a 2D of a given size, filled with a default element.
Similar to Array.repeat

    repeat 2 3 0 == [[0, 0, 0], [0, 0, 0]]
-}
repeat : Int -> Int -> a -> Array2D a
repeat numRows numColumns e =
    let
        row =
            Array.repeat numColumns e
    in
        { data = Array.repeat numRows row
        , columns = numColumns
        }


{-| Get the number of rows in an Array2D

    rows [[1, 2, 3], [4, 5, 6]] == 2
-}
rows : Array2D a -> Int
rows array2d =
    Array.length array2d.data


{-| Get the number of columns in an Array2D

    columns [[1, 2, 3], [4, 5, 6]] == 3
-}
columns : Array2D a -> Int
columns array2d =
    array2d.columns


{-| Check if an Array2D is empty.

    isEmpty [] == True
-}
isEmpty : Array2D a -> Bool
isEmpty array2d =
    Array.isEmpty array2d.data


{-| Get an individual row

    getRow 1 [[1, 2], [3, 4]] == Just [3, 4]
-}
getRow : Int -> Array2D a -> Maybe (Array a)
getRow row array2d =
    Array.get row array2d.data


{-| get column-th cell of each row as an Array

    getColumn 1 [[1, 2], [3, 4]] == [Just 2, Just 4]
-}
getColumn : Int -> Array2D a -> Array (Maybe a)
getColumn column array2d =
    Array.map
        (\rowArray -> Array.get column rowArray)
        array2d.data


{-| Get a cell.

    get 1 1 [[1, 2], [3, 4]] == Just 4
-}
get : Int -> Int -> Array2D a -> Maybe a
get row col array2d =
    getRow row array2d |> Maybe.andThen (Array.get col)


{-| Update a cell, returning the changed Array2D.

    set 0 0 -100 [[1, 2], [3, 4]] == [[-100, 2], [3, 4]]
-}
set : Int -> Int -> a -> Array2D a -> Array2D a
set row col newValue array2d =
    getRow row array2d
        |> Maybe.map (\rowAry -> { array2d | data = (Array.set row (Array.set col newValue rowAry) array2d.data) })
        |> Maybe.withDefault array2d


{-| Append a row. If the row is too long, it will be truncated,
too short and it will be expanded with filler elements.

    appendRow [3, 4] -1 [[1, 2]] == [[1, 2], [3, 4]]

    -- Filler needed for short row:
    appendRow [3] -1 [[1, 2]] == [[1, 2], [3, -1]]
-}
appendRow : Array a -> a -> Array2D a -> Array2D a
appendRow row filler array2d =
    let
        normalizedRow =
            Helpers.normalize array2d.columns filler row

        newRows =
            Array.push normalizedRow array2d.data
    in
        { array2d | data = newRows }


{-| Append a column. Filler will be used if the column length
is less than the number of rows in the Array2D. If it is longer,
it will be truncated.

    appendColumn [2, 2] -1 [[1], [1]] == [[1, 2], [1,2]]

    -- Filler needed for short column:
    appendColumn [2] -1 [[1], [1]] == [[1, 2], [1,-1]]
-}
appendColumn : Array a -> a -> Array2D a -> Array2D a
appendColumn column filler array2d =
    let
        newData =
            array2d.data
                |> Array.indexedMap
                    (\index row ->
                        let
                            newCell =
                                column |> Array.get index |> Maybe.withDefault filler
                        in
                            Array.push newCell row
                    )
    in
        { array2d
            | data = newData
            , columns = array2d.columns + 1
        }


{-| Delete a row. Does nothing if the index is out of bounds.
-}
deleteRow : Int -> Array2D a -> Array2D a
deleteRow index array2d =
    { array2d | data = Helpers.deleteArrayElt index array2d.data }


{-| Delete a column. If the index is invalid, nothing will happen.
-}
deleteColumn : Int -> Array2D a -> Array2D a
deleteColumn index array2d =
    let
        newData =
            Array.map (Helpers.deleteArrayElt index) array2d.data

        newColumns =
            newData
                |> Array.get 0
                |> Maybe.map Array.length
                |> Maybe.withDefault 0
    in
        { array2d
            | data = newData
            , columns = newColumns
        }


{-| 2D version of Array.indexedMap. First two arguments of map
    function are the row and column.

    indexedMap
        (\row column cell -> toString row)
        [[1, 2], [3, 4]]
        == [["0", "0"], ["1", "1"]]
-}
indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap fn array2d =
    let
        mappedData =
            Array.indexedMap
                (\row rowAry ->
                    (Array.indexedMap
                        (\col value -> (fn row col value))
                        rowAry
                    )
                )
                array2d.data
    in
        { array2d | data = mappedData }


{-| 2D version of Array.map.

    map
        (\cell -> toString cell)
        [[1, 2], [3, 4]]
        == [["1", "2"], ["3", "4"]]
-}
map : (a -> b) -> Array2D a -> Array2D b
map fn array2d =
    indexedMap (\_ _ val -> fn val) array2d
