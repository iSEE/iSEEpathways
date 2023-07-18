#' The FgseaEnrichmentPlot class
#'
#' The `FgseaEnrichmentPlot` is a \linkS4class{Panel} where each row represents a set of features (i.e., rows).
#' Selections in this panel can be transmitted to other row-oriented panels.
#'
#' @docType methods
#' @aliases FgseaEnrichmentPlot FgseaEnrichmentPlot-class
#' initialize,FgseaEnrichmentPlot-method
#' .cacheCommonInfo,FgseaEnrichmentPlot-method
#' .createObservers,FgseaEnrichmentPlot-method
#' .defineDataInterface,FgseaEnrichmentPlot-method
#' .fullName,FgseaEnrichmentPlot-method
#' .generateTable,FgseaEnrichmentPlot-method
#' .multiSelectionActive,FgseaEnrichmentPlot-method
#' .multiSelectionCommands,FgseaEnrichmentPlot-method
#' .multiSelectionDimension,FgseaEnrichmentPlot-method
#' .panelColor,FgseaEnrichmentPlot-method
#' .refineParameters,FgseaEnrichmentPlot-method
#' .showSelectionDetails,FgseaEnrichmentPlot-method
#'
#' @name FgseaEnrichmentPlot-class
#'
#' @examples
#' x <- FgseaEnrichmentPlot(ResultName="fgsea", PathwayId="GO:0000002")
NULL

#' @export
#' @importClassesFrom iSEE Table
setClass("FgseaEnrichmentPlot", contains="Panel",
    slots=c(
        ResultName="character",
        PathwayId="character"
    )
)

#' @export
#' @importFrom methods new
FgseaEnrichmentPlot <- function(...) {
    new("FgseaEnrichmentPlot", ...)
}

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "FgseaEnrichmentPlot", function(x) "#BB00FF")

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "FgseaEnrichmentPlot", function(x) "GSEA enrichment plot")

#' @export
#' @importMethodsFrom methods initialize
#' @importFrom methods callNextMethod
setMethod("initialize", "FgseaEnrichmentPlot", function(.Object,
    ResultName = NA_character_, PathwayId = NA_character_, ...) {
    args <- list(ResultName = ResultName, PathwayId = PathwayId, ...)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("FgseaEnrichmentPlot", function(object) {
    return(TRUE)
})

#' @export
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .getCachedCommonInfo .setCachedCommonInfo
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "FgseaEnrichmentPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "FgseaEnrichmentPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    result_names <- names(metadata(se)[["iSEEpathways"]])

    .setCachedCommonInfo(se, "FgseaEnrichmentPlot", valid.result.names = result_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .replaceMissingWithFirst
#' @importFrom methods slot
#' @importFrom S4Vectors metadata
setMethod(".refineParameters", "FgseaEnrichmentPlot", function(x, se) {
    x <- callNextMethod() # Trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    result_names <- .getCachedCommonInfo(se, "FgseaEnrichmentPlot")$valid.result.names
    x <- .replaceMissingWithFirst(x, .resultName, result_names)

    pathway_ids <- rownames(metadata(se)[["iSEEpathways"]][[slot(x, .resultName)]])
    x <- .replaceMissingWithFirst(x, .pathwayId, pathway_ids)

    x
})

#' @export
#' @importMethodsFrom iSEE .defineOutput
#' @importFrom shinyWidgets addSpinner
setMethod(".defineOutput", "FgseaEnrichmentPlot", function(x, ...){
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) {
        paste0(plot_name, "_", field)
    }
    col <- .getPanelColor(x)
    brush_stroke <- col
    brush_fill <- iSEE:::.lighten_color_for_fill(col)
    brush.opts <- brushOpts(.input_FUN(iSEE:::.brushField), resetOnNew = TRUE,
        delay = 2000, direction = "x", fill = brush_fill,
        stroke = brush_stroke, opacity = iSEE:::.brushFillOpacity)
    dblclick <- .input_FUN(iSEE:::.zoomClick)
    clickopt <- .input_FUN(iSEE:::.lassoClick)
    panel_height <- paste0(slot(x, iSEE:::.organizationHeight), "px")
    addSpinner(plotOutput(plot_name, brush = brush.opts, dblclick = dblclick,
        click = clickopt, height = panel_height),
        color = brush_fill)
})
