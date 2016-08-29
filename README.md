# 2D Array library for Elm

2D array library implemented using nested elm Arrays.
Duplicates the elm Array API as much as possible, but with
a row and column for every element. Useful for making data grids,
is it provides row and column operations in addition to getting,
setting, and mapping over cells.


```elm
let
  myAry =
    Array2D.fromList [["Row 1-Col 1", "Row 1-Col 2"], ["Row 2-Col 1", "Row 2-Col 2"]]

  changed =
    Array2D.set 0 0 "NEW VALUE" myAry -- "Row 1-Col 1" becomes "NEW VALUE"

  mappedAry =
    Array2D.indexedMap
      (\row col cell ->  cell)
      changed
in
  mappedAry -- Your app code here
```
