
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
*[iSEE](https://bioconductor.org/packages/3.16/iSEE)* applications.

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
## basic example code
```

## Citation

Below is the citation output from using `citation('iSEEpathways')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEpathways**.

``` r
print(citation('iSEEpathways'), bibtex = TRUE)
#> 
#> To cite package 'iSEEpathways' in publications use:
#> 
#>   Rue-Albrecht K (2022). _iSEEpathways: iSEE extension for panels
#>   related to pathway analysis_. R package version 0.99.0,
#>   <https://github.com/iSEE/iSEEpathways>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {iSEEpathways: iSEE extension for panels related to pathway analysis},
#>     author = {Kevin Rue-Albrecht},
#>     year = {2022},
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
  *[BiocCheck](https://bioconductor.org/packages/3.16/BiocCheck)*.
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
*[biocthis](https://bioconductor.org/packages/3.16/biocthis)*.

## Code of Conduct

Please note that the iSEEpathways project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
