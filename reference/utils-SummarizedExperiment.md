# Generics for Embbedding Pathway Analysis Results into a SummarizedExperiment Object

An overview of the generics for embedding pathway analysis results into
a
[SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
object, in a format compatible with iSEEpathways.

## Usage

``` r
embedPathwaysResults(x, se, name, pathwayType, ...)

embedPathwaysResultsMethods

# S4 method for class 'data.frame'
embedPathwaysResults(x, se, name, class, pathwayType, ...)
```

## Format

`embedPathwaysResultsMethods`: Named character vector mapping keywords
to class names designed to store pathway analysis results.

## Arguments

- x:

  Object to be embedded.

- se:

  A
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- name:

  Identifier for the embedded object.

- pathwayType:

  Character scalar indicating the type of pathway. See Details.

- ...:

  Arguments passed to individual constructors of pathway analysis result
  classes.

- class:

  Class to use for embedding `x`. Only used when `class(x)` does not
  uniquely identify the package that generated the object `x`.

## Value

`embedPathwaysResults` returns an updated
[SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
object that contains the embedded object.

## Details

The argument `pathwayType=` is used to identify the function mapping a
certain type of pathway identifier to the corresponding feature
identifiers. Pathway mapping functions must be registered as a named
list using `registerAppOptions(se, Pathways.map.functions = list(...))`,
where the name matching `pathwayType` identifies the function to use to
map pathway identifiers to feature identifiers in a given pathway
analysis result, e.g.

    library(org.Hs.eg.db)
    map_GO <- function(pathway_id) {
      mapIds(org.Hs.eg.db, pathway_id, "ENSEMBL", keytype = "GOALL", multiVals = "CharacterList")[[pathway_id]]
    }
    se <- registerAppOptions(se, Pathways.map.functions = list(GO = map_go))

See
[`vignette("integration", package = "iSEEpathways")`](https://isee.github.io/iSEEpathways/articles/integration.md)
for examples.

## Definitions

- `embedPathwaysResults(x, se, name, pathwayType, ...)` embeds the
  results `x` in the
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  `se` under the key `name`; `pathwayType` is a character scalar
  required to identify a function mapping a pathway identifier to
  asssociated feature identifiers; additional named arguments in `...`
  are passed to the constructor of the pathway results class.

## Author

Kevin Rue-Albrecht

## Examples

``` r
embedPathwaysResultsMethods
#>              fgsea 
#> "iSEEfgseaResults" 

showMethods(embedPathwaysResults)
#> Function: embedPathwaysResults (package iSEEpathways)
#> x="data.frame"
#> x="data.table"
#>     (inherited from: x="data.frame")
#> x="iSEEfgseaResults"
#> 
```
