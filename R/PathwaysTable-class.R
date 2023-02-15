#' The PathwaysTable class
#' 
#' The `PathwaysTable` is a \linkS4class{Panel} where each row represents a set of features (i.e., rows).
#' Selections in this panel can be transmitted to other row-oriented panels.
#' 
#' @docType methods
#' @aliases PathwaysTable PathwaysTable-class
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
#' @importMethodsFrom iSEE .refineParameters
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
setMethod(".generateTable", "PathwaysTable", function(x, envir) {
    cmds <-"tab <- as.data.frame(metadata(se)[['iSEEpathways']][['fgsea']]);"
    
    .textEval(cmds, envir)
    
    cmds
})

#' @export
#' @importMethodsFrom iSEE .showSelectionDetails
setMethod(".showSelectionDetails", "PathwaysTable", function(x) {
    FUN <- getAppOption("PathwaysTable.select.details")
    if (!is.null(FUN)) {
        FUN(.singleSelectionValue(x))
    }
})

#' @export
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .getEncodedName
#' @importFrom methods callNextMethod
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
