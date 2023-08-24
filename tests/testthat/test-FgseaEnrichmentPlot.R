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
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)

  out <- .cacheCommonInfo(x, se)

  expect_identical(
    .getCachedCommonInfo(out, "FgseaEnrichmentPlot")$valid.result.names,
    "fgsea"
  )
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

  expect_named(output, c(
    "FgseaEnrichmentPlot1_INTERNAL_PanelMultiSelectInfo",
    "FgseaEnrichmentPlot1",
    "FgseaEnrichmentPlot1_INTERNAL_PanelSelectLinkInfo")
)
})

test_that(".generateOutput", {
  x <- FgseaEnrichmentPlot()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
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
