# Generics for Pathway Analysis Results

An overview of the generics for accessing common pieces of information
in pathway analysis results.

## Value

`pathwayType(x)` returns a character scalar indicating the type of
pathways analysed.

`pathwaysList(x)` returns the named list of pathways used in the
analysis, or `NULL`.

`featuresStats(x)` returns the named numeric vector of feature-level
statistics used in the analysis, or `NULL`.

## Author

Kevin Rue-Albrecht

## Examples

``` r
showMethods(pathwayType)
#> Function: pathwayType (package iSEEpathways)
#> x="iSEEfgseaResults"
#> 
showMethods(pathwaysList)
#> Function: pathwaysList (package iSEEpathways)
#> x="iSEEfgseaResults"
#> 
showMethods(featuresStats)
#> Function: featuresStats (package iSEEpathways)
#> x="iSEEfgseaResults"
#> 
```
