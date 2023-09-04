
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iSEEpathways

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/iSEE/iSEEpathways)](https://github.com/iSEE/iSEEpathways/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/iSEE/iSEEpathways)](https://github.com/iSEE/iSEEpathways/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/iSEE/iSEEpathways/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/iSEE/iSEEpathways/actions)
[![Codecov test
coverage](https://codecov.io/gh/iSEE/iSEEpathways/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iSEE/iSEEpathways?branch=main)
<!-- badges: end -->

The goal of `iSEEpathways` is to provide panels to facilitate the
interactive visualisation of pathway analysis results in
*[iSEE](https://bioconductor.org/packages/3.18/iSEE)* applications.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `iSEEpathways` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("iSEEpathways")
```

And the development version from
[GitHub](https://github.com/iSEE/iSEEpathways) with:

``` r
BiocManager::install("iSEE/iSEEpathways")
```

## Example

This is a basic example which shows you how to load the package:

``` r
library("iSEEpathways")
library("fgsea")
library("iSEE")

# Example data ----

simulated_data <- simulateExampleData()

pathways_list <- simulated_data[["pathwaysList"]]
features_stat <- simulated_data[["featuresStat"]]
se <- simulated_data[["summarizedexperiment"]]

# fgsea ----

set.seed(42)
fgseaRes <- fgsea(pathways = pathways_list, 
                  stats    = features_stat,
                  minSize  = 15,
                  maxSize  = 500)
fgseaRes <- fgseaRes[order(pval), ]
head(fgseaRes)
#>         pathway         pval      padj   log2err         ES       NES size
#> 1: pathway_1350 0.0004373110 0.5905978 0.4984931  0.2858201  1.503211  299
#> 2: pathway_4907 0.0005947840 0.5905978 0.4772708  0.3250965  1.599638  178
#> 3: pathway_3983 0.0007509197 0.5905978 0.4772708  0.2558001  1.398213  451
#> 4:  pathway_398 0.0008716489 0.5905978 0.4772708  0.2799932  1.477849  305
#> 5: pathway_3359 0.0009809867 0.5905978 0.4550599 -0.3724340 -1.674911  106
#> 6: pathway_1289 0.0009835850 0.5905978 0.4550599  0.3479133  1.638048  124
#>                                                                            leadingEdge
#> 1:  feature_6060,feature_9203,feature_1852,feature_1883,feature_12903,feature_2143,...
#> 2: feature_9265,feature_6286,feature_14879,feature_9600,feature_5335,feature_12205,...
#> 3:  feature_495,feature_12466,feature_13128,feature_3069,feature_5278,feature_4248,...
#> 4:    feature_6478,feature_2164,feature_922,feature_4298,feature_6585,feature_1633,...
#> 5:    feature_3879,feature_2726,feature_6870,feature_6787,feature_9700,feature_693,...
#> 6: feature_6376,feature_12953,feature_8391,feature_3147,feature_7330,feature_11551,...

# iSEE ---

se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "simulated",
                           pathwaysList = pathways_list, featuresStats = features_stat)

app <- iSEE(se, initial = list(
  PathwaysTable(ResultName="fgsea", Selected = "pathway_3363 ", PanelWidth = 6L),
  FgseaEnrichmentPlot(ResultName="fgsea", PathwayId = "pathway_3363", PanelWidth = 6L)
))

if (interactive()) {
  shiny::runApp(app)
}
```

## Citation

Below is the citation output from using `citation('iSEEpathways')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEpathways**.

``` r
print(citation('iSEEpathways'), bibtex = TRUE)
#> To cite package 'iSEEpathways' in publications use:
#> 
#>   Rue-Albrecht K (2023). _iSEEpathways: iSEE extension for panels
#>   related to pathway analysis_. R package version 0.99.0,
#>   <https://github.com/iSEE/iSEEpathways>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {iSEEpathways: iSEE extension for panels related to pathway analysis},
#>     author = {Kevin Rue-Albrecht},
#>     year = {2023},
#>     note = {R package version 0.99.0},
#>     url = {https://github.com/iSEE/iSEEpathways},
#>   }
```

Please note that the `iSEEpathways` was only made possible thanks to
many other R and bioinformatics software authors, which are cited either
in the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `iSEEpathways` project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.18/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation website](http://iSEE.github.io/iSEEpathways) is
  automatically updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.18/biocthis)*.

## Code of Conduct

Please note that the iSEEpathways project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
