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
#' .defineInterface,FgseaEnrichmentPlot-method
#' .defineOutput,FgseaEnrichmentPlot-method
#' .fullName,FgseaEnrichmentPlot-method
#' .generateOutput,FgseaEnrichmentPlot-method
#' .isBrushable,FgseaEnrichmentPlot-method
#' .multiSelectionActive,FgseaEnrichmentPlot-method
#' .multiSelectionCommands,FgseaEnrichmentPlot-method
#' .multiSelectionDimension,FgseaEnrichmentPlot-method
#' .panelColor,FgseaEnrichmentPlot-method
#' .refineParameters,FgseaEnrichmentPlot-method
#' .renderOutput,FgseaEnrichmentPlot-method
#' .showSelectionDetails,FgseaEnrichmentPlot-method
#'
#' @name FgseaEnrichmentPlot-class
#'
#' @examples
#' x <- FgseaEnrichmentPlot(ResultName="fgsea", PathwayId="GO:0000002")
NULL

slotDefs <- character(0)

slotDefs[.resultName] <- "character"
slotDefs[.pathwayId] <- "character"

slotDefs[iSEE:::.brushData] <- "list"

#' @export
#' @importClassesFrom iSEE Table
setClass("FgseaEnrichmentPlot", contains="Panel",
    slots=slotDefs
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

    pathway_ids <- names(pathways(metadata(se)[["iSEEpathways"]][[slot(x, .resultName)]]))
    x <- .replaceMissingWithFirst(x, .pathwayId, pathway_ids)

    x
})

#' @export
#' @importMethodsFrom iSEE .defineOutput .getPanelColor
#' @importFrom shiny brushOpts plotOutput
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
#' @importFrom ggplot2 geom_rect
setMethod(".generateOutput", "FgseaEnrichmentPlot", function (x, se, ..., all_memory, all_contents)
{
    .local <- function (x, se, all_memory, all_contents) {
        pathway_id <- x[[.pathwayId]]
        if (identical(pathway_id, "")) {
            return(NULL)
        }
        plot_env <- new.env()
        plot_env$se <- se
        plot_env$colormap <- iSEE:::.get_colormap(se)
        plot_name <- iSEE::.getEncodedName(x)
        result_name <- x[[.resultName]]
        all_cmds <- list()
        # Doing this first so all_active is available in the environment
        iSEE:::.populate_selection_environment(x, plot_env)
        all_cmds$pre_cmds = paste0(c(
            sprintf('.pathways <- pathways(metadata(se)[["iSEEpathways"]][[%s]])', dQuote(result_name, FALSE)),
            sprintf('.stats <- featuresStats(metadata(se)[["iSEEpathways"]][[%s]])', dQuote(result_name, FALSE))
            ), collapse = "\n")
        plot_cmds <- sprintf('fgsea_plot <- fgsea::plotEnrichment(.pathways[[%s]], .stats)', dQuote(pathway_id, FALSE))
        if (!is.null(.multiSelectionActive(x))) {
            brush_src <- sprintf("all_active[['%s']]", plot_name)
            brush_data <- sprintf("%s[c('xmin', 'xmax', 'ymin', 'ymax')]", brush_src)
            stroke_color <- .getPanelColor(x)
            fill_color <- iSEE:::.lighten_color_for_fill(stroke_color)
            aes_call <- sprintf("xmin=%s, xmax=%s, ymin=%s, ymax=%s", 'xmin', 'xmax', 'ymin', 'ymax')
            # Build up the command that draws the brush
            brush_draw_cmd <- sprintf(
"geom_rect(aes(%s), color='%s', alpha=%s, fill='%s',
    data=do.call(data.frame, %s),
    inherit.aes=FALSE)",
                aes_call, stroke_color, iSEE:::.brushFillOpacity, fill_color, brush_data)
            plot_cmds <- paste0(plot_cmds, " +", "\n", brush_draw_cmd)
        }
        all_cmds$plot_cmds <- plot_cmds
        all_cmds$plot_data_cmds <- paste0(c(
          ".stats_rank <- rank(-.stats)",
          sprintf(".stats_rank_in_pathway <- .stats_rank[names(.stats_rank) %%in%% .pathways[[%s]]]", dQuote(pathway_id, FALSE)),
          "plot.data <- data.frame(
  rank = .stats_rank_in_pathway,
  row.names = names(.stats_rank_in_pathway)
)"
          ), collapse = "\n")
        .textEval(all_cmds, plot_env)
        list(commands = all_cmds, contents = plot_env$plot.data, plot = plot_env$fgsea_plot,
            varname = "plot.data")
    }
    .local(x, se, ..., all_memory, all_contents)
})

#' @export
#' @importMethodsFrom iSEE .defineDataInterface .selectizeInput.iSEE
#' @importFrom methods callNextMethod
#' @importFrom shiny hr
#' @importFrom iSEE .addSpecificTour .getCachedCommonInfo .getEncodedName
#' .selectInput.iSEE
setMethod(".defineDataInterface", "FgseaEnrichmentPlot", function(x, se, select_info) {
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
  # nocov start
  .addSpecificTour(class(x), .pathwayId, function(plot_name) {
    data.frame(
      rbind(
        c(
          element = paste0("#", plot_name, "_", sprintf("%s + .selectize-control", .pathwayId)),
          intro = "Here, we select the identifier of the pathway to visualise amongst the choice of pathways processed in the selected pathway analysis."
        )
      )
    )
  })
  # nocov end
  cached <- .getCachedCommonInfo(se, "FgseaEnrichmentPlot")

  list(
    .selectInput.iSEE(x, .resultName,
      label = "Result:",
      selected = x[[.resultName]],
      choices = cached$valid.result.names
    ),
    .selectizeInput.iSEE(x, .pathwayId,
      label = "Pathway:",
      choices =  NULL,
      selected = NULL,
      multiple = FALSE
    )
  )
})

#' @export
#' @importMethodsFrom iSEE .defineInterface
setMethod(".defineInterface", "FgseaEnrichmentPlot", function(x, se, select_info) {
  list(
    do.call(iSEE:::.collapseBoxHidden,
      c(
        list(x=x, field=iSEE::.dataParamBoxOpen, title="Data parameters"),
        open=slot(x, iSEE::.dataParamBoxOpen),
        iSEE::.defineDataInterface(x, se, select_info)
      )
    )
  )
})

#' @export
#' @importMethodsFrom iSEE .createObservers
#' @importFrom iSEE .createProtectedParameterObservers .getEncodedName .requestActiveSelectionUpdate
#' @importFrom methods callNextMethod slot slot<-
#' @importFrom shiny observeEvent updateSelectizeInput
setMethod(".createObservers", "FgseaEnrichmentPlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()

  plot_name <- .getEncodedName(x)
  .input_FUN <- function(field) {
    paste0(plot_name, "_", field)
  }
  # do NOT add .resultName to the protected parameters as it automatically triggers rerendering through updateSelectizeInput(.pathwayId)
  .createProtectedParameterObservers(plot_name,
                                     fields = c(.pathwayId),
                                     input = input, pObjects = pObjects, rObjects = rObjects
  )

  resultName_field <- .input_FUN(.resultName)
  pathwayId_field <- .input_FUN(.pathwayId)

  observeEvent(input[[resultName_field]], {
    current_value_resultName <- slot(pObjects$memory[[plot_name]], .resultName)
    matched_input_resultName <- as(input[[resultName_field]], typeof(current_value_resultName))
    if (!identical(matched_input_resultName, current_value_resultName)) {
      slot(pObjects$memory[[plot_name]], .resultName) <- matched_input_resultName
    }
    current_value_pathwayId <- slot(pObjects$memory[[plot_name]], .pathwayId)
    matched_input_pathwayId <- as(input[[pathwayId_field]], typeof(current_value_pathwayId))
    pathwayId_choices <- names(pathways(metadata(se)[["iSEEpathways"]][[slot(x, .resultName)]]))
    pathwayId_selected <- ifelse(matched_input_pathwayId %in% pathwayId_choices, matched_input_pathwayId, pathwayId_choices[1])
    # Update the choices of pathwayId, including when initialised
    updateSelectizeInput(session, .input_FUN(.pathwayId), choices = pathwayId_choices, selected = pathwayId_selected, server = TRUE)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  iSEE:::.create_brush_observer(plot_name, input, session, pObjects, rObjects)

  invisible(NULL)
})

#' @export
#' @importMethodsFrom iSEE .multiSelectionDimension
setMethod(".multiSelectionDimension", "FgseaEnrichmentPlot", function(x) "row")

#' @export
#' @importMethodsFrom iSEE .multiSelectionCommands
setMethod(".multiSelectionCommands", "FgseaEnrichmentPlot", function(x, index) {
    cmds <- "selected <- rownames(shiny::brushedPoints(contents, select));"
    cmds
})

#' @export
#' @importMethodsFrom iSEE .multiSelectionActive
setMethod(".multiSelectionActive", "FgseaEnrichmentPlot", function(x) {
    brush_data <- slot(x, iSEE:::.brushData)
    if (iSEE:::.is_brush(brush_data)) {
        brush_data
    } else {
        NULL
    }
})

#' @export
#' @importMethodsFrom iSEE .isBrushable
setMethod(".isBrushable", "FgseaEnrichmentPlot", function(x) TRUE)
