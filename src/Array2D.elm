{- Implements 2D array using nested Arrays. Useful for implementing data grids.
   Array2D is like a matrix in that cells are accessed by row, then column.
   Otherwise, it works very similarly to the Array class.

   Array2D's can be constructed from an Array or List of rows, where each
   row is an Array or List of cells. Behavior if the nested arrays happen
   to be jagged is currently undefined / handled poorly, so don't do this.
-}
module Array2D where

import Array exposing (Array)
import Json.Decode as Json

type alias Array2D a = { data : Array (Array a) }

-- Creating 2D arrays

empty : Array2D a
empty =
  { data = Array.empty }


fromArray : Array (Array a) -> Array2D a
fromArray array =
  { data = array }


fromList : List (List a) -> Array2D a
fromList list =
  (List.map Array.fromList list)
    |> Array.fromList
    |> fromArray


-- Get number of rows
rows : Array2D a -> Int
rows array2d =
  Array.length array2d.data


-- Get number of columns
columns : Array2D a -> Int
columns array2d =
  case getRow 0 array2d of
    Nothing -> 0
    Just xs -> Array.length xs


-- Check if 2D array is empty
isEmpty : Array2D a -> Bool
isEmpty array2d =
  Array.isEmpty array2d.data


-- Get an individual row
getRow : Int -> Array2D a -> Maybe (Array a)
getRow row array2d =
  Array.get row array2d.data


-- get columnth-th cell of each row as an Array
getColumn : Int -> Array2D a -> Array (Maybe a)
getColumn column array2d =
  Array.map
    (\rowArray -> Array.get column rowArray)
    array2d.data


-- Get an individual cell
get : Int -> Int -> Array2D a -> Maybe a
get row col array2d =
  let rowAry = getRow row array2d
  in
    case rowAry of
      Nothing -> Nothing
      Just rowAry -> Array.get col rowAry


-- Set an individual cell
set : Int -> Int -> a -> Array2D a -> Array2D a
set row col newValue array2d =
  let rowAry = getRow row array2d
  in
    case rowAry of
      Nothing -> array2d
      Just rowAry ->
        { array2d | data <- (Array.set row (Array.set col newValue rowAry) array2d.data) }


-- Add a row
appendRow : Array a -> Array2D a -> Array2D a
appendRow row array2d =
  let
    wrappedRow = Array.fromList [row] -- Need to wrap in array for Array.append
    newRows = Array.append array2d.data wrappedRow
  in
    { array2d | data <- newRows }


-- Internal helper for deleting elements from Arrays
deleteArrayElt : Int -> Array a -> Array a
deleteArrayElt index array =
  let
    first = Array.slice 0 index array
    last = Array.slice (index + 1) (Array.length array) array
  in
    Array.append first last


-- Delete a row
deleteRow : Int -> Array2D a -> Array2D a
deleteRow index array2d =
  { array2d | data <- deleteArrayElt index array2d.data }


-- Delete a column
deleteColumn : Int -> Array2D a -> Array2D a
deleteColumn index array2d =
  { array2d | data <- Array.map (deleteArrayElt index) array2d.data }


-- 2D version of Array.indexedMap. First two arguments of map function are the row and column.
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
    { array2d | data <- mappedData }


-- 2D version of Array.map. First two arguments of map function are the row and column.
map : (a -> b) -> Array2D a -> Array2D b
map fn array2d =
  indexedMap (\row col val -> fn val) array2d


-- JSON decoder for nested arrays. Be sure they are NOT jagged. Normalize first if
-- this is a risk.
decoder : Json.Decoder a -> Json.Decoder (Array2D a)
decoder cellDecoder =
  Json.object1
    Array2D
    (Json.array (Json.array cellDecoder))
