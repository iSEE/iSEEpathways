# The iSEEpathwaysResults class

The iSEEpathwaysResults is a virtual class for storing a set of pathway
analysis results.

## Value

In the following code snippets, `x` is an instance of a
`iSEEpathwaysResults` class. Refer to the documentation for each method
for more details on the remaining arguments.

- [`pathwayType`](https://isee.github.io/iSEEpathways/reference/pathway-generics.md)`(x)`
  returns the type of pathways stored in `x`.

## Slot overview

This class inherits all its slots directly from its parent class
`DataFrame`.

## Author

Kevin Rue-Albrecht

## Examples

``` r
showClass("iSEEpathwaysResults")
#> Virtual Class "iSEEpathwaysResults" [package "iSEEpathways"]
#> 
#> Slots:
#>                                                                               
#> Name:           rownames             nrows       elementType   elementMetadata
#> Class: character_OR_NULL           integer         character DataFrame_OR_NULL
#>                                           
#> Name:           metadata          listData
#> Class:              list              list
#> 
#> Extends: 
#> Class "DFrame", directly
#> Class "DataFrame", by class "DFrame", distance 2
#> Class "SimpleList", by class "DFrame", distance 2
#> Class "RectangularData", by class "DFrame", distance 3
#> Class "List", by class "DFrame", distance 3
#> Class "DataFrame_OR_NULL", by class "DFrame", distance 3
#> Class "Vector", by class "DFrame", distance 4
#> Class "list_OR_List", by class "DFrame", distance 4
#> Class "Annotated", by class "DFrame", distance 5
#> Class "vector_OR_Vector", by class "DFrame", distance 5
#> 
#> Known Subclasses: "iSEEfgseaResults"
```
