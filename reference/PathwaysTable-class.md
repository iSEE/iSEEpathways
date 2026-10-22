# The PathwaysTable class

The `PathwaysTable` is a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) where
each row represents a set of features (i.e., rows). Selections in this
panel can be transmitted to other row-oriented panels.

## Value

`PathwaysTable()` returns an object of class `PathwaysTable`.

## Slot overview

The following slots control the test procedure:

- `ResultName`, a character scalar indicating the name of the pathway
  analysis result to display.

In addition, this class inherits all slots from its parent
[Table](https://isee.github.io/iSEE/reference/Table-class.html) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) class.

## Examples

``` r
x <- PathwaysTable(ResultName="fgsea")
x
#> Panel object of class PathwaysTable
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionSource: ---
#>     DataBoxOpen: FALSE
#>     HiddenColumns: 
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     ResultName: fgsea
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionSource: ---
#>     Search: 
#>     SearchColumns: 
#>     Selected: NA
#>     SelectionBoxOpen: FALSE
#>     VersionInfo: list of length 1
```
