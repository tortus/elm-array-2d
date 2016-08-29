module Array2D exposing
  ( Array2D
  , empty, fromArray, fromList, repeat
  , rows, columns, isEmpty
  , get, set
  , getRow, appendRow, deleteRow
  , getColumn, deleteColumn
  , map, indexedMap
  )

{-| Implements 2D array using nested Arrays. Useful for implementing data grids.
Array2D is like a matrix in that cells are accessed by row, then column.
Otherwise, it works very similarly to the Array class.

Array2D's can be constructed from an Array or List of rows, where each
row is an Array or List of cells. Behavior if the nested arrays happen
to be jagged is currently undefined / handled poorly, so don't do this!

    Array2D.fromList [["Row 1-Col 1", "Row 1-Col 2"], ["Row 2-Col 1", "Row 2-Col 2"]]

@docs Array2D

# Initialization
@docs empty, fromArray, fromList, repeat

# Getting info
@docs rows, columns, isEmpty

# Fetching/updating individual cells
@docs get, set

# Adding/removing rows
@docs getRow, appendRow, deleteRow

# Adding/removing columns
@docs getColumn, deleteColumn

# Mapping cell data
@docs map, indexedMap
-}


import Array exposing (Array)
import Maybe exposing (andThen)


{-| Base Array2D type -}
type alias Array2D a = { data : Array (Array a) }


{-| Create an empty Array2D -}
empty : Array2D a
empty =
  { data = Array.empty }


{-| Create an Array2D from an Array of Arrays.

    let row1 = Array.fromList [1, 2]
        row2 = Array.fromList [2, 3]
    in
      fromArray (Array.fromList [row1, row2])
-}
fromArray : Array (Array a) -> Array2D a
fromArray array =
  { data = array }


{-| Create an Array2D from a List of Lists.

    fromList [[1, 2, 3], [4, 5, 6]]
-}
fromList : List (List a) -> Array2D a
fromList list =
  (List.map Array.fromList list)
    |> Array.fromList
    |> fromArray


{-| Create a 2D of a given size, filled with a default element.
Similar to Array.repeat

    repeat 2 3 0 == [[0, 0, 0], [0, 0, 0]]
-}
repeat : Int -> Int -> a -> Array2D a
repeat numRows numColumns e =
  let
    row = Array.repeat numColumns e
  in
    { data = Array.repeat numRows row }


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
  getRow 0 array2d |> Maybe.map Array.length |> Maybe.withDefault 0


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
  getRow row array2d `andThen` Array.get col


{-| Update a cell, returning the changed Array2D.

    set 0 0 -100 [[1, 2], [3, 4]] == [[-100, 2], [3, 4]]
-}
set : Int -> Int -> a -> Array2D a -> Array2D a
set row col newValue array2d =
  getRow row array2d
    |> Maybe.map (\rowAry -> { array2d | data = (Array.set row (Array.set col newValue rowAry) array2d.data) })
    |> Maybe.withDefault array2d


{-| Append a row

    appendRow [3, 4] [[1, 2]] == [[1, 2], [3, 4]]
-}
appendRow : Array a -> Array2D a -> Array2D a
appendRow row array2d =
  let
    wrappedRow = Array.fromList [row] -- Need to wrap in array for Array.append
    newRows = Array.append array2d.data wrappedRow
  in
    { array2d | data = newRows }


-- Internal helper for deleting elements from Arrays
deleteArrayElt : Int -> Array a -> Array a
deleteArrayElt index array =
  let
    first = Array.slice 0 index array
    last = Array.slice (index + 1) (Array.length array) array
  in
    Array.append first last


{-| Delete a row -}
deleteRow : Int -> Array2D a -> Array2D a
deleteRow index array2d =
  { array2d | data = deleteArrayElt index array2d.data }


{-| Delete a column -}
deleteColumn : Int -> Array2D a -> Array2D a
deleteColumn index array2d =
  { array2d | data = Array.map (deleteArrayElt index) array2d.data }


{-| 2D version of Array.indexedMap. First two arguments of map function are the row and column.

    indexedMap (\row column cell -> toString row) [[1, 2], [3, 4]] == [["0", "0"], ["1", "1"]]
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

    map (\cell -> toString cell) [[1, 2], [3, 4]] == [["1", "2"], ["3", "4"]]
-}
map : (a -> b) -> Array2D a -> Array2D b
map fn array2d =
  indexedMap (\row col val -> fn val) array2d
