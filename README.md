# 2D Array library for Elm

2D array library implemented using nested elm Arrays.
Duplicates the elm Array API as much as possible, but with
a row and column for every element. Useful for making data grids,
is it provides row and column operations in addition to getting,
setting, and mapping over cells.

See the module docs for the complete list of operations: http://package.elm-lang.org/packages/tortus/elm-array-2d/2.0.1/Array2D


```elm
let
  array2d =
    Array2D.fromList
      [ ["Row 1-Col 1", "Row 1-Col 2"]
      , ["Row 2-Col 1", "Row 2-Col 2"]
      ]

  changed =
    -- "Row 1-Col 1" becomes "NEW VALUE"
    Array2D.set 0 0 "NEW VALUE" array2d

  mappedArray =
    Array2D.indexedMap
      (\row col cell -> cell)
      changed
in
  -- Your app code here
  { model | dataGrid = mappedArray }
```

## Drawbacks and caveats

Most examples of nested models in Elm use Lists of elements with a
unique, constant ID. This allows messages to always be routed to the correct
element, even if element are re-ordered, removed, added, etc.
If you use the index of the element instead and then create a Task that
takes time to complete during which the element's index changes, the completion
message may be applied to the wrong list element.
