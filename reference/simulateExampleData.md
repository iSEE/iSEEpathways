# Simulate Example Data

Simulates various pieces of data for the purpose of demonstration in
vignettes and help pages.

## Usage

``` r
simulateExampleData(
  n_pathways = 5000,
  n_features = 15000,
  n_samples = 8,
  pathway_sizes = 15:500
)
```

## Arguments

- n_pathways:

  integer scalar, number of pathways to simulate.

- n_features:

  integer scalar, number of features to simulate.

- n_samples:

  integer scalar, number of samples to simulate.

- pathway_sizes:

  integer vector, possible sizes of pathway to sample from.

## Value

A list of three elements:

- pathwaysList:

  A named list of dummy pathways. Names represent pathway identifiers;
  values represent character vectors of feature identifiers.

- featuresStat:

  A named numeric vector of dummy feature-wise statistics. Names
  represent feature identifiers; values represent 'scores' (e.g., log2
  fold-change)

- summarizedexperiment:

  A
  [SummarizedExperiment](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object that contains a count matrix with rownames and colnames.

## Details

At least for the time being, this function generates dummy data purely
for the purpose of demonstrating the *format* of expected inputs.

As such, the independent pieces of simulated data are just that –
independent – in the meaning that simulated counts, statistics, and
pathways are not related numerically, and do not make any biological
sense.

The only coherent piece of information is the set of feature
identifiers, carefully coordinated between the rownames of the count
matrix, the names of the feature statistics, and the set of features in
the list of pathways, so that panels in the app can transmit and
interpret that shared piece of information.

## Examples

``` r
set.seed(1)
simulated_data <- simulateExampleData()
head(lengths(simulated_data$pathwaysList))
#> pathway_1 pathway_2 pathway_3 pathway_4 pathway_5 pathway_6 
#>       338       181       143       432       485       313 
head(simulated_data$featuresStat)
#>   feature_1   feature_2   feature_3   feature_4   feature_5   feature_6 
#>  1.17063084 -1.39634072  0.07569717  0.22496577 -0.94038886  0.58330266 
```
