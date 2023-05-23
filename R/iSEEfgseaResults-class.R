#' The iSEEpathwaysResults class
#'
#' The iSEEpathwaysResults class represents an undefined resource.
#'
#' @section Slot overview:
#' \itemize{
#' \item \code{pathwayType}, a character scalar specifying the type of pathway (e.g., `"GO"`).
#' }
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
#' @name iSEEpathwaysResults-class
#' @rdname iSEEpathwaysResults-class
#' @aliases
#' show,iSEEpathwaysResults-method
#' pathwayType,iSEEpathwaysResults-method
#'
#' @examples
#' new("iSEEpathwaysResults", pathwayType = "GO")
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
#' This class inherits all its slots directly from its parent class \linkS4class{DataFrame}.
#'
#' @section Constructor:
#' \code{iSEEfgseaResults(data, row.names = rownames(data))} creates an instance of a `iSEEfgseaResults` class, with:
#'
#' \describe{
#' \item{`data`}{A `data.frame` produced by `fgsea::fgsea()`.}
#' }
#'
#' @section Supported methods:
#' \itemize{
#' \item `embedPathwaysResults(x, se, name, pathwayType, class = "fgsea", ...)` embeds `x` in the column `name` of `metadata(se)[["iSEEpathways"]]`.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @name iSEEfgseaResults-class
#' @aliases
#' iSEEfgseaResults
#' embedPathwaysResults,iSEEfgseaResults-method
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
#' # iSEEfgseaResults ----
#' ##
#'
#' # Simulate the original SummarizedExperiment object
#' se <- SummarizedExperiment()
#'
#'
#' se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "GO")
#'
#' se
#'
#' ##
#' # Methods ----
#' ##
#'
#' ## TODO
NULL

setClass("iSEEfgseaResults", contains = "iSEEpathwaysResults")

#' @export
#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
iSEEfgseaResults <- function(data, pathwayType) {
    metadata <- list(pathwayType = pathwayType)
    out <- new("iSEEfgseaResults", data,
               metadata = metadata)
    validObject(out)
    out
}

#' @export
setMethod("pathwayType", "iSEEfgseaResults", function(x) {
    out <- metadata(x)[["pathwayType"]]
    out
})
