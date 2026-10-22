# Extract contrast results embedded in a SummarizedExperiment object

`pathwaysResults` returns either all pathway analysis results stored in
`object` or a single pathway analysis result by name.

`pathwaysResultsNames` returns the names of pathway analysis results
embedded in `object`.

## Usage

``` r
pathwaysResultsNames(object)

pathwaysResults(object, name)
```

## Arguments

- object:

  A
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- name:

  (Optional) Name of a single pathway analysis result name to extract.
  Use `pathwaysResultsNames(object)` to list available names.

## Value

For `pathwaysResultsNames`: the names of embedded contrast results
available.

For `pathwaysResults`: a `list` of differential expression statistics.

If `name` is missing, `pathwaysResults` returns a `list` in which each
item contains the results of a single pathway analysis. If `name` is
given, `pathwaysResults` returns a `DataFrame` that contains the results
of a single pathway analysis.

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
#>      pathway      pval      padj    log2err         ES        NES  size
#>       <char>     <num>     <num>      <num>      <num>      <num> <int>
#> 1: pathway_2 0.2190202 0.9006024 0.15964670  0.3232076  1.1843141    39
#> 2: pathway_1 0.5102639 0.9006024 0.06252374 -0.2922706 -0.9558240    79
#> 3: pathway_4 0.5779528 0.9006024 0.05998925 -0.2849186 -0.9145010    32
#> 4: pathway_3 0.7876506 0.9006024 0.04362512 -0.2704209 -0.7935048    88
#> 5: pathway_5 0.9006024 0.9006024 0.06994587  0.1875875  0.7245837    63
#>                                                                  leadingEdge
#>                                                                       <list>
#> 1:  feature_83,feature_80,feature_57,feature_47,feature_94,feature_25,...[9]
#> 2:  feature_73,feature_22,feature_4,feature_75,feature_36,feature_68,...[37]
#> 3:  feature_22,feature_11,feature_18,feature_54,feature_28,feature_90,...[9]
#> 4: feature_73,feature_75,feature_36,feature_68,feature_31,feature_11,...[45]
#> 5:   feature_83,feature_80,feature_23,feature_47,feature_94,feature_7,...[7]

##
# iSEEfgseaResults ----
##

se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "GO",
  pathwaysList = pathways_list, featuresStats = features_stats)

##
# List result names ---
##

pathwaysResultsNames(se)
#> [1] "fgsea"

##
# Extract results ---
##

pathwaysResults(se)
#> $fgsea
#> iSEEfgseaResults with 5 rows and 7 columns
#>                pval      padj   log2err        ES       NES      size
#>           <numeric> <numeric> <numeric> <numeric> <numeric> <integer>
#> pathway_1  0.510264  0.900602 0.0625237 -0.292271 -0.955824        79
#> pathway_2  0.219020  0.900602 0.1596467  0.323208  1.184314        39
#> pathway_3  0.787651  0.900602 0.0436251 -0.270421 -0.793505        88
#> pathway_4  0.577953  0.900602 0.0599892 -0.284919 -0.914501        32
#> pathway_5  0.900602  0.900602 0.0699459  0.187588  0.724584        63
#>                                    leadingEdge
#>                                         <list>
#> pathway_1  feature_73,feature_22,feature_4,...
#> pathway_2 feature_83,feature_80,feature_57,...
#> pathway_3 feature_73,feature_75,feature_36,...
#> pathway_4 feature_22,feature_11,feature_18,...
#> pathway_5 feature_83,feature_80,feature_23,...
#> 
pathwaysResults(se, "fgsea")
#> iSEEfgseaResults with 5 rows and 7 columns
#>                pval      padj   log2err        ES       NES      size
#>           <numeric> <numeric> <numeric> <numeric> <numeric> <integer>
#> pathway_1  0.510264  0.900602 0.0625237 -0.292271 -0.955824        79
#> pathway_2  0.219020  0.900602 0.1596467  0.323208  1.184314        39
#> pathway_3  0.787651  0.900602 0.0436251 -0.270421 -0.793505        88
#> pathway_4  0.577953  0.900602 0.0599892 -0.284919 -0.914501        32
#> pathway_5  0.900602  0.900602 0.0699459  0.187588  0.724584        63
#>                                    leadingEdge
#>                                         <list>
#> pathway_1  feature_73,feature_22,feature_4,...
#> pathway_2 feature_83,feature_80,feature_57,...
#> pathway_3 feature_73,feature_75,feature_36,...
#> pathway_4 feature_22,feature_11,feature_18,...
#> pathway_5 feature_83,feature_80,feature_23,...
```
