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
#' \item `embedResults(x, se, name, class = "fgsea", ...)` embeds `x` in the column `name` of `metadata(se)[["iSEEpathways"]]`.
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
#' # Package the results in a iSEEfgseaResults object
#' iseefgsea_table <- iSEEfgseaResults(fgseaRes)
#' 
#' se <- embedPathwaysResults(iseefgsea_table, se, name = "fgsea")
#'
#' se
#'
#' ##
#' # Methods ----
#' ##
#'
#' ## TODO
NULL

setClass("iSEEfgseaResults", contains = "DFrame")

#' @export
#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
iSEEfgseaResults <- function(data, row.names = rownames(data)) {
    df <- DataFrame(row.names = row.names)
    df[rownames(data), colnames(data)] <- data
    new("iSEEfgseaResults", df)
}
