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
#'   stats    = features_stats,
#'   minSize  = 15,
#'   maxSize  = 500)
#' head(fgseaRes[order(pval), ])
#'
#' ##
#' # iSEEfgseaResults ----
#' ##
#'
#' se <- embedPathwaysResults(fgseaRes, se, name = "fgsea", class = "fgsea", pathwayType = "GO",
#'   pathwaysList = pathways_list, featuresStats = features_stats)
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
