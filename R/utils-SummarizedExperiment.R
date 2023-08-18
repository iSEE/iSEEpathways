#' @export
#'
#' @format
#' `embedPathwaysResultsMethods`: Named character vector mapping keywords to class names designed to store pathway analysis results.
#'
#' @rdname utils-SummarizedExperiment
embedPathwaysResultsMethods <- c(
    "fgsea" = "iSEEfgseaResults"
)

#' @importFrom S4Vectors DataFrame metadata metadata<-
.embed_pathway_result <- function(x, se, name) {
    iseepathways_data <- metadata(se)[["iSEEpathways"]]
    if (is.null(iseepathways_data)){
        iseepathways_data <- list()
    }
    if (name %in% names(iseepathways_data)) {
        msg <- sprintf(
            "Results already exist under name %s.
            Replacing with new results.\n",
            sQuote(name)
        )
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    iseepathways_data[[name]] <- x
    metadata(se)[["iSEEpathways"]] <- iseepathways_data
    se
}

#' @export
#'
#' @param ... Arguments passed to individual constructors of pathway analysis result classes.
#'
#' @rdname utils-SummarizedExperiment
#' @aliases embedPathwaysResults,data.frame-method
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
setMethod("embedPathwaysResults", "data.frame", function(x, se, name, class, pathwayType, ...) {
    if (!class %in% names(embedPathwaysResultsMethods)) {
        msg <- sprintf(
            "argument %s must be a value in %s,
      for signature %s.",
            sQuote("class"),
            sQuote("names(embedPathwaysResultsMethods)"),
            sQuote("x=data.frame")
        )
        stop(paste(strwrap(msg), collapse = "\n"))
    }
    constructor <- get(embedPathwaysResultsMethods[class])
    res <- constructor(x, pathwayType = pathwayType, ...)
    embedPathwaysResults(res, se, name, ...)
})

#' @export
setMethod("embedPathwaysResults", "iSEEfgseaResults", function(x, se, name, ...) {
    .embed_pathway_result(x, se, name)
})

# pathwaysResultsNames ----

#' @export
#' @rdname pathwaysResults
pathwaysResultsNames <- function(object){
  names(metadata(object)[["iSEEpathways"]])
}

#' Extract contrast results embedded in a SummarizedExperiment object
#'
#' @description
#' `pathwaysResults` returns either all pathway analysis results stored in `object`
#' or a single pathway analysis result by name.
#'
#' `pathwaysResultsNames` returns the names of pathway analysis results embedded in `object`.
#'
#' @param object A [SummarizedExperiment-class] object.
#' @param name (Optional) Name of a single pathway analysis result name to extract.
#' Use `pathwaysResultsNames(object)` to list available names.
#'
#' @return
#' For `pathwaysResultsNames`: the names of embedded contrast results available.
#'
#' For `pathwaysResults`: a `list` of differential expression statistics.
#'
#' If `name` is missing, `pathwaysResults` returns a `list` in which each item contains the results of a single pathway analysis.
#' If `name` is given, `pathwaysResults` returns a [`DataFrame-class`] that contains the results of a single pathway analysis.
#'
#' @export
#'
#' @examples
#' #' library("iSEEpathways")
#' library("org.Hs.eg.db")
#' library("fgsea")
#' library("iSEE")
#'
#' # Example data ----
#'
#' ## Pathways
#' pathways <- select(org.Hs.eg.db, keys(org.Hs.eg.db, "SYMBOL"), c("GOALL"), keytype = "SYMBOL")
#' pathways <- subset(pathways, ONTOLOGYALL == "BP")
#' pathways <- unique(pathways[, c("SYMBOL", "GOALL")])
#' pathways <- split(pathways$SYMBOL, pathways$GOALL)
#' len_pathways <- lengths(pathways)
#' pathways <- pathways[len_pathways > 15 & len_pathways < 500]
#'
#' ## Features
#' set.seed(1)
#' # simulate a score for all genes found across all pathways
#' feature_stats <- rnorm(length(unique(unlist(pathways))))
#' names(feature_stats) <- unique(unlist(pathways))
#' # arbitrarily select a pathway to simulate enrichment
#' pathway_id <- "GO:0046324"
#' pathway_genes <- pathways[[pathway_id]]
#' # increase score of genes in the selected pathway to simulate enrichment
#' feature_stats[pathway_genes] <- feature_stats[pathway_genes] + 1
#'
#' # fgsea ----
#'
#' set.seed(42)
#' fgseaRes <- fgsea(pathways = pathways,
#'   stats    = feature_stats,
#'   minSize  = 15,
#'   maxSize  = 500)
#' head(fgseaRes[order(pval), ])
#'
#' # iSEE ---
#'
#' ngenes <- length(feature_stats)
#' cnts <- matrix(rnbinom(n=ngenes*2, mu=100, size=1/0.5), nrow=ngenes)
#' rownames(cnts) <- names(feature_stats)
#' se <- SummarizedExperiment(assay = list(counts = cnts))
#'
#' se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "GO",
#'   pathwaysList = pathways, featuresStats = feature_stats)
#'
#' ##
#' # List result names ---
#' ##
#'
#' pathwaysResultsNames(se)
#'
#' ##
#' # Extract results ---
#' ##
#'
#' pathwaysResults(se)
#' pathwaysResults(se, "fgsea")
pathwaysResults <- function(object, name) {
  results_list <- metadata(object)[["iSEEpathways"]]

  if (missing(name)) {
    return(results_list)
  }

  if (!name %in% names(results_list)) {
    msg <- sprintf(
      "'%s' is not a valid pathway result name,
      use pathwaysResultsNames(object) to list valid names.",
      name
    )
    stop(paste(strwrap(msg), collapse = "\n"))
  }

  results_list <- results_list[[name]]
  return(results_list)

}
