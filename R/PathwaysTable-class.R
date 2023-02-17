#' The PathwaysTable class
#'
#' The `PathwaysTable` is a \linkS4class{Panel} where each row represents a set of features (i.e., rows).
#' Selections in this panel can be transmitted to other row-oriented panels.
#'
#' @docType methods
#' @aliases PathwaysTable PathwaysTable-class
#' initialize,PathwaysTable-method
#' .createObservers,PathwaysTable-method
#' .fullName,PathwaysTable-method
#' .generateTable,PathwaysTable-method
#' .multiSelectionActive,PathwaysTable-method
#' .multiSelectionCommands,PathwaysTable-method
#' .multiSelectionDimension,PathwaysTable-method
#' .panelColor,PathwaysTable-method
#' .refineParameters,PathwaysTable-method
#' .showSelectionDetails,PathwaysTable-method
#'
#' @name PathwaysTable-class
#'
#' @examples
#' x <- PathwaysTable(ResultName="fgsea")
NULL

#' @export
#' @importClassesFrom iSEE Table
setClass("PathwaysTable", contains="Table",
         slots=c(
             ResultName="character"
         )
)

#' @export
#' @importFrom methods new
PathwaysTable <- function(...) {
    new("PathwaysTable", ...)
}

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "PathwaysTable", function(x) "#BB00FF")

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "PathwaysTable", function(x) "Pathways Analysis Table")

#' @export
#' @importMethodsFrom methods initialize
#' @importFrom methods callNextMethod
setMethod("initialize", "PathwaysTable", function(.Object,
                                                  ResultName = NA_character_, ...) {
    args <- list(ResultName = ResultName, ...)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .replaceMissingWithFirst
#' @importFrom S4Vectors metadata
setMethod(".refineParameters", "PathwaysTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, iSEE:::.TableSelected, rownames(metadata(se)[["iSEEpathways"]][["fgsea"]]))

    x
})

#' @export
#' @importMethodsFrom iSEE .generateTable
#' @importFrom iSEE .textEval
setMethod(".generateTable", "PathwaysTable", function(x, envir) {
    cmds <-"tab <- as.data.frame(metadata(se)[['iSEEpathways']][['fgsea']]);"

    .textEval(cmds, envir)

    cmds
})

#' @export
#' @importMethodsFrom iSEE .showSelectionDetails
#' @importFrom iSEE getAppOption .singleSelectionValue
setMethod(".showSelectionDetails", "PathwaysTable", function(x) {
    FUN <- getAppOption("PathwaysTable.select.details")
    if (!is.null(FUN)) {
        FUN(.singleSelectionValue(x))
    }
})

#' @export
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .getEncodedName .requestActiveSelectionUpdate
#' @importFrom methods callNextMethod
#' @importFrom shiny observeEvent
setMethod(".createObservers", "PathwaysTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()
    panel_name <- .getEncodedName(x)
    single_name <- paste0(panel_name, "_", iSEE:::.flagSingleSelect)

    select_field <- paste0(panel_name, iSEE:::.int_statTableSelected)
    observeEvent(input[[select_field]], {
        # Single-selection reactive updates the selection details
        iSEE:::.safe_reactive_bump(rObjects, single_name)
        # trigger re-rendering of downstream panels
        .requestActiveSelectionUpdate(panel_name, session=session, pObjects=pObjects,
                                      rObjects=rObjects, update_output=FALSE)
    })
})

#' @export
#' @importMethodsFrom iSEE .multiSelectionDimension
setMethod(".multiSelectionDimension", "PathwaysTable", function(x) "row")

#' @export
#' @importMethodsFrom iSEE .multiSelectionCommands
setMethod(".multiSelectionCommands", "PathwaysTable", function(x, index) {
    # TODO: replace hard-coded 'GO'; dynamically detect class of pathway analysis results
    c(
        sprintf(".pathway_id <- %s;", deparse(x[["Selected"]])),
        'FUN <- getAppOption("Pathways.map.functions", se)[["GO"]]',
        "selected <- FUN(.pathway_id, se)"
    )
})

#' @export
#' @importMethodsFrom iSEE .multiSelectionActive
setMethod(".multiSelectionActive", "PathwaysTable", function(x) {
    if (nzchar(x[["Selected"]])) {
        x[["Selected"]]
    } else {
        NULL
    }
})
