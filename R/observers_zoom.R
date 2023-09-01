#' Double-click observer
#' 
#' Double-clicking anywhere in the panel clears an existing brush.
#' 
#' @details
#' Adapted from `iSEE:::.create_zoom_observer()`.
#' 
#' @param plot_name String containing the name of the panel.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' 
#' @rdname INTERNAL_create_double_click_observer
#'
#' @importFrom iSEE .requestUpdate
.create_double_click_observer <- function(plot_name, input, session, pObjects, rObjects) {
  dblclick_field <- paste0(plot_name, "_", iSEE:::.zoomClick)
  
  # nocov start
  observeEvent(input[[dblclick_field]], {
    existing_brush <- slot(pObjects$memory[[plot_name]], iSEE:::.brushData)
    # Zooming destroys all active brushes or lassos.
    slot(pObjects$memory[[plot_name]], iSEE:::.brushData) <- list()
    
    # While re-creating the plot clears the brush, it doesn't
    # re-trigger the observer as the observer ignores NULLs.
    # So we have to manually retrigger the downstream effects.
    if (iSEE:::.is_brush(existing_brush)) {
      iSEE:::.mark_panel_as_modified(plot_name, iSEE:::.panelReactivated, rObjects)
    } else {
      .requestUpdate(plot_name, rObjects)
    }
  }, ignoreInit=TRUE)
  # nocov end
  
  invisible(NULL)
}
