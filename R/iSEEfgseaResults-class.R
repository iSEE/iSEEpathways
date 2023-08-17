#' The iSEEpathwaysResults class
#'
#' The iSEEpathwaysResults is a virtual class for storing a set of pathway analysis results.
#'
#' @section Slot overview:
#' This class inherits all its slots directly from its parent class `DataFrame`.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a [`iSEEpathwaysResults-class`] class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' \itemize{
#' \item \code{\link{pathwayType}(x)} returns the type of pathways stored in `x`.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @aliases
#' iSEEpathwaysResults
#' show,iSEEpathwaysResults-method
#'
#' @name iSEEpathwaysResults-class
#'
#' @examples
#' showClass("iSEEpathwaysResults")
NULL

#' @export
setClass("iSEEpathwaysResults",
         contains = c("DFrame", "VIRTUAL"))

setValidity2("iSEEpathwaysResults", function(object) {
    msg <- NULL

    pathwayType <- metadata(object)[["pathwayType"]]
    if (!is(pathwayType, "character")) {
        msg <- c(msg, "metadata(object)[['pathwayType']] must be a character scalar")
    }
    if (!identical(length(pathwayType), 1L)) {
        msg <- c(msg, "metadata(object)[['pathwayType']] must be length 1")
    }

    if (length(msg)) {
        return(msg)
    }

    return(TRUE)
})

#' The iSEEfgseaResults class
#'
#' The `iSEEfgseaResults` class is used to provide an common interface to pathway analysis results produced by the \pkg{fgsea} package.
#' It provides methods to access the set of features in each pathway.
#'
#' @section Slot overview:
#' This class inherits all its slots directly from its parent class `iSEEpathwaysResults`.
#'
#' @section Constructor:
#' \code{iSEEfgseaResults(data, pathwayType, pathwaysList = NULL, featuresStats = NULL)} creates an instance of a `iSEEfgseaResults` class, with:
#'
#' \describe{
#' \item{`data`}{A `data.frame` produced by `fgsea::fgsea()`.}
#' \item{`pathwayType`}{A character scalar specifying the type of pathway (e.g., `"GO"`). See [embedPathwaysResults].}
#' \item{`pathwaysList`}{A named list of pathways and associated feature identifiers.}
#' \item{`featuresStats`}{Feature-level statistics used in the pathway analysis.}
#' }
#'
#' @section Supported methods:
#' In the following code snippets, `x` is an instance of a `iSEEfgseaResults` class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' \itemize{
#' \item `embedPathwaysResults(x, se, name, pathwayType, ...)` embeds `x` in se under the identifier `name`. See `embedPathwaysResults()` for more details.
#' \item `pathwaysList(x)` returns the named list of pathway identifiers and associated
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @name iSEEfgseaResults-class
#' @aliases
#' iSEEfgseaResults iSEEfgseaResults-class
#' embedPathwaysResults,iSEEfgseaResults-method
#' featuresStats,iSEEfgseaResults-method
#' pathwaysList,iSEEfgseaResults-method
#' pathwayType,iSEEfgseaResults-method
#'
#' @examples
#' library("org.Hs.eg.db")
#' library("fgsea")
#' library("SummarizedExperiment")
#'
#' ##
#' # Prepare pathways
#' ##
#'
#' pathways <- select(org.Hs.eg.db, keys(org.Hs.eg.db, "SYMBOL"), c("GOALL"), keytype = "SYMBOL")
#' pathways <- subset(pathways, ONTOLOGYALL == "BP")
#' pathways <- unique(pathways[, c("SYMBOL", "GOALL")])
#' pathways <- split(pathways$SYMBOL, pathways$GOALL)
#'
#' ##
#' # Prepare gene statistics
#' ##
#' gene_symbols <- unique(unlist(pathways))
#' gene_stats <- rnorm(length(gene_symbols))
#' names(gene_stats) <- gene_symbols
#'
#' ##
#' # Run pathway analysis ----
#' ##
#'
#' set.seed(42)
#' fgseaRes <- fgsea(pathways = pathways,
#'                   stats    = gene_stats,
#'                   minSize  = 15,
#'                   maxSize  = 500)
#' head(fgseaRes[order(pval), ])
#'
#' ##
#' # Simulate a SummarizedExperiment object
#' ##
#'
#' se <- SummarizedExperiment()
#'
#' ##
#' # iSEEfgseaResults ----
#' ##
#'
#' # Embed the FGSEA results in the SummarizedExperiment object
#' se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea",
#'   pathwayType = "GO", pathwaysList = pathways, featuresStats = gene_stats)
#' se
#'
#' ##
#' # Access ----
#' ##
#'
#' pathwaysResultsNames(se)
#' pathwaysResults(se)
#' pathwaysResults(se, "fgsea")
#'
#' pathwayType(pathwaysResults(se, "fgsea"))
#' head(lengths(pathwaysList(pathwaysResults(se, "fgsea"))))
#' head(featuresStats(pathwaysResults(se, "fgsea")))
NULL

setClass("iSEEfgseaResults", contains = "iSEEpathwaysResults")

#' @export
#' @importFrom methods as new validObject
#' @importFrom S4Vectors DataFrame
iSEEfgseaResults <- function(data, pathwayType, pathwaysList = NULL, featuresStats = NULL) {
    data <- as(data, "DataFrame")
    # TODO: throw error if column 'pathway' not found
    rownames(data) <- data$pathway
    data$pathway <- NULL
    metadata <- list(
      pathwayType = pathwayType,
      pathwaysList = pathwaysList,
      featuresStats = featuresStats
    )
    out <- new("iSEEfgseaResults", data,
               metadata = metadata)
    validObject(out)
    out
}

#' @export
#' @importFrom S4Vectors metadata
setMethod("pathwayType", "iSEEfgseaResults", function(x) {
    out <- metadata(x)[["pathwayType"]]
    out
})

#' @export
#' @importFrom S4Vectors metadata
setMethod("pathwaysList", "iSEEfgseaResults", function(x) {
  out <- metadata(x)$pathwaysList
  out
})

#' @export
#' @importFrom S4Vectors metadata
setMethod("featuresStats", "iSEEfgseaResults", function(x) {
  out <- metadata(x)[["featuresStats"]]
  out
})
