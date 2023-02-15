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
