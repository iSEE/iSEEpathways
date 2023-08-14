
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
library("org.Hs.eg.db")
library("fgsea")
library("iSEE")

# Example data ----

## Pathways
pathways <- select(org.Hs.eg.db, keys(org.Hs.eg.db, "SYMBOL"), c("GOALL"), keytype = "SYMBOL")
pathways <- subset(pathways, ONTOLOGYALL == "BP")
pathways <- unique(pathways[, c("SYMBOL", "GOALL")])
pathways <- split(pathways$SYMBOL, pathways$GOALL)
len_pathways <- lengths(pathways)
pathways <- pathways[len_pathways > 15 & len_pathways < 500]

## Features
set.seed(1)
# simulate a score for all genes found across all pathways
feature_stats <- rnorm(length(unique(unlist(pathways))))
names(feature_stats) <- unique(unlist(pathways))
# arbitrarily select a pathway to simulate enrichment
pathway_id <- "GO:0046324"
pathway_genes <- pathways[[pathway_id]]
# increase score of genes in the selected pathway to simulate enrichment
feature_stats[pathway_genes] <- feature_stats[pathway_genes] + 1

# fgsea ----

set.seed(42)
fgseaRes <- fgsea(pathways = pathways, 
                  stats    = feature_stats,
                  minSize  = 15,
                  maxSize  = 500)
head(fgseaRes[order(pval), ])
#>       pathway         pval         padj   log2err        ES      NES size
#> 1: GO:0046323 1.174458e-10 4.609944e-07 0.8266573 0.5982413 2.555596   75
#> 2: GO:0008645 1.840665e-10 4.609944e-07 0.8266573 0.5115897 2.367116  117
#> 3: GO:0010827 4.296846e-10 5.194149e-07 0.8140358 0.5797781 2.475614   77
#> 4: GO:1904659 4.524314e-10 5.194149e-07 0.8140358 0.5117531 2.365294  114
#> 5: GO:0046324 5.184816e-10 5.194149e-07 0.8012156 0.6314064 2.542207   59
#> 6: GO:0015749 6.491366e-10 5.419208e-07 0.8012156 0.4997745 2.325238  120
#>                                    leadingEdge
#> 1:   PTPN11,TSC1,CREBL2,PTH,MIR107,SELENOS,...
#> 2:    PTPN11,SLC2A8,TSC1,CREBL2,PTH,MIR107,...
#> 3: PTPN11,CREBL2,PTH,MIR107,SELENOS,SLC1A2,...
#> 4:    PTPN11,SLC2A8,TSC1,CREBL2,PTH,MIR107,...
#> 5: PTPN11,CREBL2,PTH,MIR107,SELENOS,SLC1A2,...
#> 6:    PTPN11,SLC2A8,TSC1,CREBL2,PTH,MIR107,...

# iSEE ---

ngenes <- length(feature_stats)
cnts <- matrix(rnbinom(n=ngenes*2, mu=100, size=1/0.5), nrow=ngenes)
rownames(cnts) <- names(feature_stats)
se <- SummarizedExperiment(assay = list(counts = cnts))

se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "GO",
                           pathways = pathways, stats = feature_stats)

app <- iSEE(se, initial = list(
  PathwaysTable(ResultName="fgsea", PanelWidth = 6L),
  FgseaEnrichmentPlot(ResultName="fgsea", PathwayId="GO:0000002", PanelWidth = 6L)
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
