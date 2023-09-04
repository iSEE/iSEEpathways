# library(testthat); library(iSEEpathways); library(SummarizedExperiment)

test_that("FgseaEnrichmentPlot constructor works", {
  out <- FgseaEnrichmentPlot()

  expect_s4_class(out, "FgseaEnrichmentPlot")
  expect_identical(out[["ResultName"]], NA_character_)
  expect_identical(out[["PathwayId"]], NA_character_)

  out <- FgseaEnrichmentPlot(ResultName="fgsea", PathwayId="GO:0000002")
  expect_s4_class(out, "FgseaEnrichmentPlot")
  expect_identical(out[["ResultName"]], "fgsea")
  expect_identical(out[["PathwayId"]], "GO:0000002")
})

test_that(".panelColor works", {
  x <- FgseaEnrichmentPlot()

  out <- .panelColor(x)
  expect_identical(out, "#BB00FF")
})

test_that(".fullName works", {
  x <- FgseaEnrichmentPlot()

  out <- .fullName(x)
  expect_identical(out, "GSEA enrichment plot")
})

test_that(".cacheCommonInfo works in the absence of results", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()

  out <- .cacheCommonInfo(x, se)

  expect_null(.getCachedCommonInfo(out, "FgseaEnrichmentPlot")$valid.result.names)
})

test_that(".cacheCommonInfo works in the presence of results", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)

  out <- .cacheCommonInfo(x, se)

  expect_identical(
    .getCachedCommonInfo(out, "FgseaEnrichmentPlot")$valid.result.names,
    "fgsea"
  )

  # run it again to simulate multiple instances of the panel class
  out <- .cacheCommonInfo(x, out)

  expect_identical(
    .getCachedCommonInfo(out, "FgseaEnrichmentPlot")$valid.result.names,
    "fgsea"
  )
})

test_that(".refineParameters works as expected", {
  # bypass method dispatch to test on NULL
  FUN <- getMethod(".refineParameters", "FgseaEnrichmentPlot")
  out <- FUN(NULL, se)

  expect_null(out)

  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)
  se <- .cacheCommonInfo(x, se)

  out <- .refineParameters(x, se)

  expect_s4_class(out, "FgseaEnrichmentPlot")
  expect_identical(out[["ResultName"]], "fgsea")
  expect_identical(out[["PathwayId"]], "pathway_1")
})

test_that(".defineOutput returns a shiny tag list", {
  x <- FgseaEnrichmentPlot()

  out <- .defineOutput(x)

  expect_s3_class(out, "shiny.tag.list")
})

test_that(".renderOutput populates output with the expected named objects", {
  x <- FgseaEnrichmentPlot(PanelId = 1L)
  se <- SummarizedExperiment()
  output <- new.env()
  pObjects <- new.env()
  pObjects <- new.env()

  out <- .renderOutput(x, se, output = output, pObjects = pObject, rObjects = rObjects)

  expect_null(out)

  expect_named(output, c(
    "FgseaEnrichmentPlot1_INTERNAL_PanelMultiSelectInfo",
    "FgseaEnrichmentPlot1",
    "FgseaEnrichmentPlot1_INTERNAL_PanelSelectLinkInfo")
  )
  for (i in names(output)) {
    expect_s3_class(output[[i]], "shiny.render.function")
  }
})

test_that(".generateOutput works with valid choices in the absence of brush", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)
  se <- .cacheCommonInfo(x, se)
  x <- .refineParameters(x, se)
  x[["PathwayId"]] <- ""

  out <- .generateOutput(x, se, all_memory = NULL, all_contents = NULL)

  expect_null(out)
})

test_that(".generateOutput works with valid choices in the absence of brush", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)
  se <- .cacheCommonInfo(x, se)
  x <- .refineParameters(x, se)

  out <- .generateOutput(x, se, all_memory = NULL, all_contents = NULL)

  expect_named(out, c("commands", "contents", "plot", "varname"))

  expect_type(out$commands, "list")

  expect_s3_class(out$contents, "data.frame")
  expect_named(out$contents, "rank")

  expect_s3_class(out$plot, "ggplot")

  expect_identical(out$varname, "plot.data")
})

test_that(".generateOutput works with valid choices in the presence of brush", {
  x <- FgseaEnrichmentPlot(BrushData = list(xmin=1, xmax=25, ymin=-0.25, ymax=0.25))
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)
  se <- .cacheCommonInfo(x, se)
  x <- .refineParameters(x, se)

  out <- .generateOutput(x, se, all_memory = NULL, all_contents = NULL)

  expect_named(out, c("commands", "contents", "plot", "varname"))

  expect_type(out$commands, "list")

  expect_s3_class(out$contents, "data.frame")
  expect_named(out$contents, "rank")

  expect_s3_class(out$plot, "ggplot")

  expect_identical(out$varname, "plot.data")
})

test_that(".defineDataInterface returns a list of UI elements", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()

  out <- .defineDataInterface(x, se, select_info = NULL)

  expect_type(out, "list")
  for (i in seq_along(out)) {
    expect_s3_class(out[[i]], "shiny.tag")
  }
})

test_that(".defineInterface returns a list of shiny tag lists", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()

  out <- .defineInterface(x, se, select_info = NULL)

  expect_type(out, "list")
  for (i in seq_along(out)) {
    expect_s3_class(out[[i]], "shiny.tag.list")
  }
})

test_that(".createObservers poplulates output with the expected named objects", {
  x <- FgseaEnrichmentPlot(PanelId = 1L)
  se <- SummarizedExperiment()
  input <- NULL
  session <- NULL
  pObjects <- new.env()
  rObjects <- new.env()

  out <- .createObservers(x = x, se = se, input = input, session = session, pObjects = pObjects, rObjects = rObjects)

  expect_null(out)

  expect_named(rObjects, c(
    "FgseaEnrichmentPlot1_INTERNAL_saved_choices",
    "FgseaEnrichmentPlot1_INTERNAL_single_select",
    "FgseaEnrichmentPlot1",
    "FgseaEnrichmentPlot1_INTERNAL_multi_select",
    "FgseaEnrichmentPlot1_INTERNAL_relinked_select"))
  for (i in names(rObjects)) {
    expect_type(rObjects[[i]], "integer")
  }
})

test_that(".multiSelectionDimension return 'row'", {
  x <- FgseaEnrichmentPlot()

  out <- .multiSelectionDimension(x)

  expect_identical(out, "row")
})

test_that(".multiSelectionCommands returns a character vector", {
  x <- FgseaEnrichmentPlot()

  out <- .multiSelectionCommands(x)

  expect_type(out, "character")
})

test_that(".multiSelectionActive returns the current brush data", {
  x <- FgseaEnrichmentPlot()

  out <- .multiSelectionActive(x)
  expect_null(out)

  x <- FgseaEnrichmentPlot(BrushData = list(dummy_data = NA))

  out <- .multiSelectionActive(x)
  expect_type(out, "list")
})

test_that(".isBrushable returns TRUE", {
  x <- FgseaEnrichmentPlot()

  expect_true(.isBrushable(x))
})
