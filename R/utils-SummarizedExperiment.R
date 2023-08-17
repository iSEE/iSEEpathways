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
