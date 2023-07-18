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

#' @export
#' @importMethodsFrom iSEE .renderOutput
#' @importFrom iSEE .retrieveOutput
setMethod(".renderOutput", "FgseaEnrichmentPlot", function (x, se, ..., output, pObjects, rObjects)  {
    .local <- function (x, se, output, pObjects, rObjects)
    {
        plot_name <- .getEncodedName(x)
        force(se)
        output[[plot_name]] <- renderPlot({
            .retrieveOutput(plot_name, se, pObjects, rObjects)$plot
        })
        callNextMethod()
    }
    .local(x, se, ..., output, pObjects, rObjects)
})

#' @export
#' @importMethodsFrom iSEE .generateOutput
#' @importFrom iSEE .textEval
setMethod(".generateOutput", "FgseaEnrichmentPlot", function (x, se, ..., all_memory, all_contents)
{
    message(".generateOutput - FgseaEnrichmentPlot")
    # .local <- function (x, se, all_memory, all_contents)
    # {
    #     plot_env <- new.env()
    #     plot_env$se <- se
    #     plot_env$colormap <- .get_colormap(se)
    #     all_cmds <- list()
    #     all_labels <- list()
    #     all_cmds$select <- .processMultiSelections(x, all_memory,
    #         all_contents, plot_env)
    #     xy_out <- .generateDotPlotData(x, plot_env)
    #     all_cmds$xy <- xy_out$commands
    #     all_labels <- c(all_labels, xy_out$labels)
    #     extra_out <- .add_extra_aesthetic_columns(x, plot_env)
    #     all_cmds <- c(all_cmds, extra_out$commands)
    #     all_labels <- c(all_labels, extra_out$labels)
    #     select_out2 <- .add_selectby_column(x, plot_env)
    #     all_cmds <- c(all_cmds, select_out2)
    #     all_cmds$setup <- .choose_plot_type(plot_env)
    #     panel_data <- plot_env$plot.data
    #     scramble_cmds <- c("# Avoid visual biases from default ordering by shuffling the points",
    #         sprintf("set.seed(%i);", nrow(panel_data)), "plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];")
    #     .textEval(scramble_cmds, plot_env)
    #     all_cmds$shuffle <- scramble_cmds
    #     priority_out <- .prioritizeDotPlotData(x, plot_env)
    #     rescaled_res <- FALSE
    #     if (has_priority <- !is.null(priority_out)) {
    #         order_cmds <- "plot.data <- plot.data[order(.priority),,drop=FALSE];"
    #         .textEval(order_cmds, plot_env)
    #         all_cmds$priority <- c(priority_out$commands, order_cmds)
    #         rescaled_res <- priority_out$rescaled
    #     }
    #     all_cmds$downsample <- .downsample_points(x, plot_env,
    #         priority = has_priority, rescaled = rescaled_res)
    #     plot_out <- .generateDotPlot(x, all_labels, plot_env)
    #     all_cmds$plot <- plot_out$commands
    #     list(commands = all_cmds, contents = panel_data, plot = plot_out$plot,
    #         varname = "plot.data")
    # }
    .local <- function (x, se, all_memory, all_contents) {
        plot_env <- new.env()
        plot_env$se <- se
        plot_env$colormap <- iSEE:::.get_colormap(se)
        plot_env$pathways <- pathways
        plot_env$feature_stats <- feature_stats
        # TODO: add 'feature_stats' and 'pathways' to 'se'
        # TODO: fetch 'feature_stats' and 'pathways' from 'plot_env'
        # TODO: use x[[.pathwayId]]
        all_cmds <- list(
            sprintf('fgsea_plot <- fgsea::plotEnrichment(pathways[[%s]], feature_stats)', dQuote("GO:0000002", q = FALSE))
        )
        print(all_cmds)
        panel_data <- data.frame(
            X = feature_stats[rownames(se)],
            row.names = rownames(se)
        )
        print(head(panel_data))
        .textEval(all_cmds, plot_env)
        print(plot_env)
        print(ls(plot_env))
        print(class(plot_env$fgsea_plot))
        list(commands = all_cmds, contents = panel_data, plot = plot_env$fgsea_plot,
            varname = "plot.data")
    }
    .local(x, se, ..., all_memory, all_contents)
})
