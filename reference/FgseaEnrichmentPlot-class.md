# The FgseaEnrichmentPlot class

The `FgseaEnrichmentPlot` is a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) subclass
where each row represents a set of features (i.e., rows). Selections in
this panel can be transmitted to other row-oriented panels.

## Value

`FgseaEnrichmentPlot()` returns an object of class
`FgseaEnrichmentPlot`.

In addition, this class inherits all slots from its parent
[Panel](https://isee.github.io/iSEE/reference/Panel-class.html) class.

## Slot overview

The following slots control the test procedure:

- `ResultName`, a character scalar indicating the name of the pathway
  analysis result to display.

- `PathwayId`, a character scalar indicating the identifier of the
  pathway to display.

- `BrushData`, a list containing a Shiny brush along the x-axis (see
  `?brushedPoints`). Defaults to an empty list, i.e., no brush or lasso.

## Examples

``` r
x <- FgseaEnrichmentPlot(ResultName="fgsea", PathwayId="GO:0000002")
x
#> Panel object of class FgseaEnrichmentPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     BrushData: 
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionRestrict: FALSE
#>     ColumnSelectionSource: ---
#>     DataBoxOpen: FALSE
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     PathwayId: GO:0000002
#>     ResultName: fgsea
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionRestrict: FALSE
#>     RowSelectionSource: ---
#>     SelectionBoxOpen: FALSE
#>     SelectionHistory: 
#>     VersionInfo: list of length 1
```
