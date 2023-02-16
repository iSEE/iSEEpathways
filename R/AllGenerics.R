#' Generics for Embbedding Pathway Analysis Results into a SummarizedExperiment Object
#'
#' An overview of the generics for embedding pathway analysis results into a \linkS4class{SummarizedExperiment} object, in a format compatible with \pkg{iSEEpathways}.
#'
#' @section Definitions:
#' \itemize{
#' \item `embedPathwaysResults(x, se, name, ...)` embeds the results `x` in the \linkS4class{SummarizedExperiment} `se`.
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
    function(x, se, name, ...) standardGeneric("embedPathwaysResults")
)
