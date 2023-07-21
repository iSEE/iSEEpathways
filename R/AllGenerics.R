#' Generics for Pathway Analysis Results
#'
#' An overview of the generics for accessing common pieces of information in pathway analysis results.
#'
#' @section Definitions:
#' \itemize{
#' \item `pathwayType(x)` returns a character scalar indicating the type of pathways analysed.
#' \item `pathways(x)` returns NULL or the named list of pathways used in the analysis.
#' \item `featuresStats(x)` returns NULL or the named numeric vector of feature-level statistics used in the analysis.
#' }
#'
#' @docType methods
#' @aliases pathwayType
#' @name pathway-generics
#' @author Kevin Rue-Albrecht
#'
#' @examples
#' showMethods(pathwayType)
NULL

setGeneric(
    "pathwayType",
    function(x) standardGeneric("pathwayType")
)

setGeneric(
  "pathways",
  function(x) standardGeneric("pathways")
)

setGeneric(
  "featuresStats",
  function(x) standardGeneric("featuresStats")
)

#' Generics for Embbedding Pathway Analysis Results into a SummarizedExperiment Object
#'
#' An overview of the generics for embedding pathway analysis results into a \linkS4class{SummarizedExperiment} object, in a format compatible with \pkg{iSEEpathways}.
#'
#' @section Definitions:
#' \itemize{
#' \item `embedPathwaysResults(x, se, name, pathwayType, ...)` embeds the results `x` in the \linkS4class{SummarizedExperiment} `se`.
#' }
#'
#' @docType methods
#' @aliases embedPathwaysResults
#' @name utils-SummarizedExperiment
#' @author Kevin Rue-Albrecht
#'
#' @examples
#' embedPathwaysResultsMethods
#'
#' showMethods(embedPathwaysResults)
NULL

#' @rdname utils-SummarizedExperiment
#' @aliases embedPathwaysResults,ANY-method
setGeneric(
    "embedPathwaysResults",
    function(x, se, name, pathwayType, ...) standardGeneric("embedPathwaysResults")
)
