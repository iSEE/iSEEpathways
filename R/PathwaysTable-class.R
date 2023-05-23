#' The PathwaysTable class
#'
#' The `PathwaysTable` is a \linkS4class{Panel} where each row represents a set of features (i.e., rows).
#' Selections in this panel can be transmitted to other row-oriented panels.
#'
#' @docType methods
#' @aliases PathwaysTable PathwaysTable-class
#' initialize,PathwaysTable-method
#' .cacheCommonInfo,PathwaysTable-method
#' .createObservers,PathwaysTable-method
#' .defineDataInterface,PathwaysTable-method
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

#' @importFrom S4Vectors setValidity2
setValidity2("PathwaysTable", function(object) {
    return(TRUE)
})

#' @export
#' @importMethodsFrom iSEE .cacheCommonInfo
#' @importFrom iSEE .getCachedCommonInfo .setCachedCommonInfo
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "PathwaysTable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "PathwaysTable"))) {
        return(se)
    }

    se <- callNextMethod()

    result_names <- names(metadata(se)[["iSEEpathways"]])

    .setCachedCommonInfo(se, "PathwaysTable", valid.result.names = result_names)
})

#' @export
#' @importMethodsFrom iSEE .refineParameters
#' @importFrom iSEE .replaceMissingWithFirst
#' @importFrom methods slot
#' @importFrom S4Vectors metadata
setMethod(".refineParameters", "PathwaysTable", function(x, se) {
    x <- callNextMethod() # Trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    result_names <- .getCachedCommonInfo(se, "PathwaysTable")$valid.result.names
    x <- .replaceMissingWithFirst(x, .resultName, result_names)

    pathway_ids <- rownames(metadata(se)[["iSEEpathways"]][[slot(x, .resultName)]])
    x <- .replaceMissingWithFirst(x, iSEE:::.TableSelected, pathway_ids)

    x
})

#' @export
#' @importMethodsFrom iSEE .generateTable
#' @importFrom iSEE .textEval
setMethod(".generateTable", "PathwaysTable", function(x, envir) {
    cmds <- sprintf("tab <- as.data.frame(metadata(se)[['iSEEpathways']][[%s]]);", deparse(x[[.resultName]]))

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
#' @importFrom iSEE .createProtectedParameterObservers .getEncodedName .requestActiveSelectionUpdate
#' @importFrom methods callNextMethod
#' @importFrom shiny observeEvent
setMethod(".createObservers", "PathwaysTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
                                       fields = c(.resultName),
                                       input = input, pObjects = pObjects, rObjects = rObjects
    )

    single_name <- paste0(plot_name, "_", iSEE:::.flagSingleSelect)

    select_field <- paste0(plot_name, iSEE:::.int_statTableSelected)
    observeEvent(input[[select_field]], {
        # Single-selection reactive updates the selection details
        iSEE:::.safe_reactive_bump(rObjects, single_name)
        # trigger re-rendering of downstream panels
        .requestActiveSelectionUpdate(plot_name, session=session, pObjects=pObjects,
                                      rObjects=rObjects, update_output=FALSE)
    })

    invisible(NULL)
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .addSpecificTour .getCachedCommonInfo .getEncodedName
#' .selectInput.iSEE
setMethod(".defineDataInterface", "PathwaysTable", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)
    # nocov start
    .addSpecificTour(class(x), .resultName, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element = paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .resultName)),
                    intro = "Here, we select the name of the result to visualise amongst the choice of pathway analysis results available."
                )
            )
        )
    })
    # nocov end
    cached <- .getCachedCommonInfo(se, "PathwaysTable")

    extra_inputs <- list(
        .selectInput.iSEE(x, .resultName,
                          label = "Result:",
                          selected = x[[.resultName]],
                          choices = cached$valid.result.names
        )
    )

    c(
        callNextMethod(),
        list(hr()),
        extra_inputs
    )
})

#' @export
#' @importMethodsFrom iSEE .multiSelectionDimension
setMethod(".multiSelectionDimension", "PathwaysTable", function(x) "row")

#' @export
#' @importMethodsFrom iSEE .multiSelectionCommands
setMethod(".multiSelectionCommands", "PathwaysTable", function(x, index) {
    # NOTE: 'index' is unused as x[["Selected"]] is used instead
    c(
        sprintf(".pathway_id <- %s;", deparse(x[["Selected"]])),
        sprintf(".pathway_type <- iSEEpathways::pathwayType(metadata(airway)[['iSEEpathways']][[%s]])", deparse(x[[.resultName]])),
        'FUN <- getAppOption("Pathways.map.functions", se)[[.pathway_type]]',
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
