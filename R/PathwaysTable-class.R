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
