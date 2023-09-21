#' Generics for Pathway Analysis Results
#'
#' An overview of the generics for accessing common pieces of information in pathway analysis results.
#'
#' @return
#' `pathwayType(x)` returns a character scalar indicating the type of pathways analysed.
#'
#' `pathwaysList(x)` returns the named list of pathways used in the analysis, or `NULL`.
#'
#' `featuresStats(x)` returns the named numeric vector of feature-level statistics used in the analysis, or `NULL`.
#'
#' @docType methods
#' @aliases featuresStats pathwayType pathwaysList
#' @name pathway-generics
#' @author Kevin Rue-Albrecht
#'
#' @examples
#' showMethods(pathwayType)
#' showMethods(pathwaysList)
#' showMethods(featuresStats)
NULL

setGeneric(
  "pathwayType",
  function(x) standardGeneric("pathwayType")
)

setGeneric(
  "pathwaysList",
  function(x) standardGeneric("pathwaysList")
)

setGeneric(
  "featuresStats",
  function(x) standardGeneric("featuresStats")
)

#' Generics for Embbedding Pathway Analysis Results into a SummarizedExperiment Object
#'
#' An overview of the generics for embedding pathway analysis results into a \linkS4class{SummarizedExperiment} object,
#' in a format compatible with \pkg{iSEEpathways}.
#'
#' @section Definitions:
#' \itemize{
#' \item `embedPathwaysResults(x, se, name, pathwayType, ...)` embeds
#' the results `x` in the \linkS4class{SummarizedExperiment} `se` under the key `name`;
#' `pathwayType` is a character scalar required to identify a function mapping
#' a pathway identifier to asssociated feature identifiers;
#' additional named arguments in `...` are passed to the constructor of the pathway results class.
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
#' @export
#'
#' @param x Object to be embedded.
#' @param se A \linkS4class{SummarizedExperiment} object.
#' @param name Identifier for the embedded object.
#' @param class Class to use for embedding `x`. Only used when `class(x)` does
#' not uniquely identify the package that generated the object `x`.
#' @param pathwayType Character scalar indicating the type of pathway. See Details.
#'
#' @details
#' The argument `pathwayType=` is used to identify the function mapping a certain type of pathway identifier to the corresponding feature identifiers.
#' Pathway mapping functions must be registered as a named list using `registerAppOptions(se, Pathways.map.functions = list(...))`,
#' where the name matching `pathwayType` identifies the function to use to map pathway identifiers to feature identifiers in a given pathway analysis result,
#' e.g.
#' ```
#' library(org.Hs.eg.db)
#' map_GO <- function(pathway_id) {
#'   mapIds(org.Hs.eg.db, pathway_id, "ENSEMBL", keytype = "GOALL", multiVals = "CharacterList")[[pathway_id]]
#' }
#' se <- registerAppOptions(se, Pathways.map.functions = list(GO = map_go))
#' ```
#'
#' See `vignette("integration", package = "iSEEpathways")` for examples.
#'
#' @return
#' `embedPathwaysResults` returns an updated \linkS4class{SummarizedExperiment} object that contains the
#' embedded object.
#'
setGeneric(
  "embedPathwaysResults",
  function(x, se, name, pathwayType, ...) standardGeneric("embedPathwaysResults")
)
