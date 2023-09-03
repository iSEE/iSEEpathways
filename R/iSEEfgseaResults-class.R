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

setValidity2("iSEEpathwaysResults", function(.Object) {
    msg <- NULL

    pathwayType <- pathwayType(.Object)
    if (!is(pathwayType, "character")) {
        msg <- c(msg,
          "pathwayType(.Object) must be a character scalar")
    }
    if (!identical(length(pathwayType), 1L)) {
        msg <- c(msg,
          "pathwayType(.Object) must be length 1")
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
#' library("fgsea")
#'
#' ##
#' # Simulate example data
#' ##
#'
#' simulated_data <- simulateExampleData(n_pathways = 5, n_features = 100, pathway_sizes = 15:100)
#'
#' pathways_list <- simulated_data$pathwaysList
#' features_stats <- simulated_data$featuresStat
#' se <- simulated_data$summarizedexperiment
#'
#' ##
#' # Run pathway analysis ----
#' ##
#'
#' set.seed(42)
#' fgseaRes <- fgsea(pathways = pathways_list,
#'                   stats    = features_stats,
#'                   minSize  = 15,
#'                   maxSize  = 500)
#' head(fgseaRes[order(pval), ])
#'
#' ##
#' # iSEEfgseaResults ----
#' ##
#'
#' # Embed the FGSEA results in the SummarizedExperiment object
#' se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea",
#'   pathwayType = "simulated", pathwaysList = pathways_list, featuresStats = features_stats)
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
    stopifnot("pathway" %in% colnames(data))
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
