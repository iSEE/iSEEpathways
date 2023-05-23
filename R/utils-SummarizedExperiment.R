#' @export
#'
#' @rdname utils-SummarizedExperiment
embedPathwaysResultsMethods <- c(
    "fgsea" = "iSEEfgseaResults"
)

#' @importFrom SummarizedExperiment metadata<-
#' @importFrom S4Vectors DataFrame metadata
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
#' @param x Object to be embedded.
#' @param se A \linkS4class{SummarizedExperiment} object.
#' @param name Identifier for the embedded object.
#' @param class Class to use for embedding `x`. Only used when `class(x)` does
#' not uniquely identify the package that generated the object.
#' @param pathwayType Type of pathway.
#' @param ... Arguments passed to and from other methods.
#'
#' @return An updated \linkS4class{SummarizedExperiment} object that contains the
#' embedded object.
#'
#' @rdname utils-SummarizedExperiment
#' @aliases embedPathwaysResults,data.frame-method
setMethod("embedPathwaysResults", "data.frame", function(x, se, name, class, pathwayType, ...) {
    if (!class %in% names(embedPathwaysResultsMethods)) {
        msg <- sprintf(
            "argument %s must be a value in %s,
      for signature %s.",
      sQuote("class"), sQuote("names(embedPathwaysResultsMethods)"),
      sQuote("x=data.frame")
        )
        stop(paste(strwrap(msg), collapse = "\n"))
    }
    constructor <- get(embedPathwaysResultsMethods[class])
    res <- constructor(x, pathwayType = pathwayType)
    embedPathwaysResults(res, se, name, ...)
})

#' @export
setMethod("embedPathwaysResults", "iSEEfgseaResults", function(x, se, name, ...) {
    .embed_pathway_result(x, se, name)
})
