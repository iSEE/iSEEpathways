# The iSEEfgseaResults class

The `iSEEfgseaResults` class is used to provide an common interface to
pathway analysis results produced by the fgsea package. It provides
methods to access the set of features in each pathway.

## Value

`iSEEfgseaResults()` returns an object of class `iSEEfgseaResults`.

## Slot overview

This class inherits all its slots directly from its parent class
`iSEEpathwaysResults`.

## Constructor

`iSEEfgseaResults(data, pathwayType, pathwaysList = NULL, featuresStats = NULL)`
creates an instance of a `iSEEfgseaResults` class, with:

- `data`:

  A `data.frame` produced by
  [`fgsea::fgsea()`](https://rdrr.io/pkg/fgsea/man/fgsea.html).

- `pathwayType`:

  A character scalar specifying the type of pathway (e.g., `"GO"`). See
  [embedPathwaysResults](https://isee.github.io/iSEEpathways/reference/utils-SummarizedExperiment.md).

- `pathwaysList`:

  A named list of pathways and associated feature identifiers.

- `featuresStats`:

  Feature-level statistics used in the pathway analysis.

## Supported methods

In the following code snippets, `x` is an instance of a
`iSEEfgseaResults` class. Refer to the documentation for each method for
more details on the remaining arguments.

- `embedPathwaysResults(x, se, name, pathwayType, ...)` embeds `x` in se
  under the identifier `name`. See
  [`embedPathwaysResults()`](https://isee.github.io/iSEEpathways/reference/utils-SummarizedExperiment.md)
  for more details.

- `pathwaysList(x)` returns the named list of pathway identifiers and
  associated

## Author

Kevin Rue-Albrecht

## Examples

``` r
library("fgsea")

##
# Simulate example data
##

simulated_data <- simulateExampleData(n_pathways = 5, n_features = 100, pathway_sizes = 15:100)

pathways_list <- simulated_data$pathwaysList
features_stats <- simulated_data$featuresStat
se <- simulated_data$summarizedexperiment

##
# Run pathway analysis ----
##

set.seed(42)
fgseaRes <- fgsea(pathways = pathways_list,
                  stats    = features_stats,
                  minSize  = 15,
                  maxSize  = 500)
head(fgseaRes[order(pval), ])
#>      pathway       pval      padj    log2err         ES        NES  size
#>       <char>      <num>     <num>      <num>      <num>      <num> <int>
#> 1: pathway_4 0.08244681 0.2657005 0.25720647 -0.3611377 -1.3229180    77
#> 2: pathway_3 0.10628019 0.2657005 0.21392786 -0.4161521 -1.3310039    90
#> 3: pathway_2 0.50923483 0.7062500 0.09344492 -0.2666742 -0.9727191    37
#> 4: pathway_5 0.56500000 0.7062500 0.08455574 -0.2459574 -0.9463482    61
#> 5: pathway_1 0.73299748 0.7329975 0.07130530 -0.2175310 -0.8397135    59
#>                                                                 leadingEdge
#>                                                                      <list>
#> 1: feature_42,feature_36,feature_84,feature_10,feature_80,feature_4,...[18]
#> 2: feature_42,feature_36,feature_84,feature_10,feature_80,feature_4,...[24]
#> 3: feature_42,feature_36,feature_66,feature_32,feature_2,feature_23,...[17]
#> 4: feature_42,feature_84,feature_10,feature_96,feature_32,feature_2,...[24]
#> 5: feature_84,feature_10,feature_4,feature_96,feature_32,feature_67,...[22]

##
# iSEEfgseaResults ----
##

# Embed the FGSEA results in the SummarizedExperiment object
se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea",
  pathwayType = "simulated", pathwaysList = pathways_list, featuresStats = features_stats)
se
#> class: SummarizedExperiment 
#> dim: 100 8 
#> metadata(1): iSEEpathways
#> assays(1): counts
#> rownames(100): feature_1 feature_2 ... feature_99 feature_100
#> rowData names(0):
#> colnames(8): sample_1 sample_2 ... sample_7 sample_8
#> colData names(0):

##
# Access ----
##

pathwaysResultsNames(se)
#> [1] "fgsea"
pathwaysResults(se)
#> $fgsea
#> iSEEfgseaResults with 5 rows and 7 columns
#>                pval      padj   log2err        ES       NES      size
#>           <numeric> <numeric> <numeric> <numeric> <numeric> <integer>
#> pathway_1 0.7329975  0.732997 0.0713053 -0.217531 -0.839713        59
#> pathway_2 0.5092348  0.706250 0.0934449 -0.266674 -0.972719        37
#> pathway_3 0.1062802  0.265700 0.2139279 -0.416152 -1.331004        90
#> pathway_4 0.0824468  0.265700 0.2572065 -0.361138 -1.322918        77
#> pathway_5 0.5650000  0.706250 0.0845557 -0.245957 -0.946348        61
#>                                    leadingEdge
#>                                         <list>
#> pathway_1  feature_84,feature_10,feature_4,...
#> pathway_2 feature_42,feature_36,feature_66,...
#> pathway_3 feature_42,feature_36,feature_84,...
#> pathway_4 feature_42,feature_36,feature_84,...
#> pathway_5 feature_42,feature_84,feature_10,...
#> 
pathwaysResults(se, "fgsea")
#> iSEEfgseaResults with 5 rows and 7 columns
#>                pval      padj   log2err        ES       NES      size
#>           <numeric> <numeric> <numeric> <numeric> <numeric> <integer>
#> pathway_1 0.7329975  0.732997 0.0713053 -0.217531 -0.839713        59
#> pathway_2 0.5092348  0.706250 0.0934449 -0.266674 -0.972719        37
#> pathway_3 0.1062802  0.265700 0.2139279 -0.416152 -1.331004        90
#> pathway_4 0.0824468  0.265700 0.2572065 -0.361138 -1.322918        77
#> pathway_5 0.5650000  0.706250 0.0845557 -0.245957 -0.946348        61
#>                                    leadingEdge
#>                                         <list>
#> pathway_1  feature_84,feature_10,feature_4,...
#> pathway_2 feature_42,feature_36,feature_66,...
#> pathway_3 feature_42,feature_36,feature_84,...
#> pathway_4 feature_42,feature_36,feature_84,...
#> pathway_5 feature_42,feature_84,feature_10,...

pathwayType(pathwaysResults(se, "fgsea"))
#> [1] "simulated"
head(lengths(pathwaysList(pathwaysResults(se, "fgsea"))))
#> pathway_1 pathway_2 pathway_3 pathway_4 pathway_5 
#>        59        37        90        77        61 
head(featuresStats(pathwaysResults(se, "fgsea")))
#>   feature_1   feature_2   feature_3   feature_4   feature_5   feature_6 
#>  0.23742535 -1.31281425  0.74702859 -1.56251843  0.07105336 -0.63953477 
```
