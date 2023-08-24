# library(testthat); library(iSEEpathways); library(SummarizedExperiment)

test_that("PathwaysTable constructor works", {
  out <- PathwaysTable()

  expect_s4_class(out, "PathwaysTable")
  expect_identical(out[["ResultName"]], NA_character_)

  out <- PathwaysTable(ResultName="fgsea")
  expect_s4_class(out, "PathwaysTable")
  expect_identical(out[["ResultName"]], "fgsea")
})

test_that(".panelColor works", {
  x <- PathwaysTable()

  out <- .panelColor(x)
  expect_identical(out, "#BB00FF")
})

test_that(".fullName works", {
  x <- PathwaysTable()

  out <- .fullName(x)
  expect_identical(out, "Pathways Analysis Table")
})

test_that(".cacheCommonInfo works in the absence of results", {
  x <- PathwaysTable()
  se <- SummarizedExperiment()

  out <- .cacheCommonInfo(x, se)

  expect_null(.getCachedCommonInfo(out, "PathwaysTable")$valid.result.names)
})

test_that(".cacheCommonInfo works in the presence of results", {
  x <- PathwaysTable()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)

  out <- .cacheCommonInfo(x, se)

  expect_identical(
    .getCachedCommonInfo(out, "PathwaysTable")$valid.result.names,
    "fgsea"
  )

  # run it again to simulate multiple instances of the panel class
  out <- .cacheCommonInfo(x, out)

  expect_identical(
    .getCachedCommonInfo(out, "PathwaysTable")$valid.result.names,
    "fgsea"
  )
})

test_that(".refineParameters works as expected", {
  # bypass method dispatch to test on NULL
  FUN <- getMethod(".refineParameters", "PathwaysTable")
  out <- FUN(NULL, se)

  expect_null(out)

  x <- PathwaysTable()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- .cacheCommonInfo(x, se)

  out <- .refineParameters(x, se)

  expect_s4_class(out, "PathwaysTable")
  expect_identical(out[["ResultName"]], "fgsea")
})

test_that(".generateTable populates the evaluation environment and returns the commands", {
  x <- PathwaysTable()
  se <- SummarizedExperiment()
  se <- embedPathwaysResults(fgsea_result, se, name = "fgsea", class = "fgsea", pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- .cacheCommonInfo(x, se)
  x <- .refineParameters(x, se)
  envir <- new.env()
  envir$se <- se

  out <- .generateTable(x = x, envir = envir)

  expect_type(out, "character")
  expect_named(envir, c("tab", "se"))
})

test_that("showSelectionDetails", {
  x <- PathwaysTable()

  expect_null(.showSelectionDetails(x))

  se <- SummarizedExperiment()
  se <- registerAppOptions(se, PathwaysTable.select.details = function(x) "dummy_value")
  .activateAppOptionRegistry(se)

  out <- .showSelectionDetails(x)

  expect_identical(out, "dummy_value")
})

test_that(".createObservers poplulates output with the expected named objects", {
  x <- PathwaysTable(PanelId = 1L)
  se <- SummarizedExperiment()
  input <- NULL
  session <- NULL
  pObjects <- new.env()
  rObjects <- new.env()

  out <- .createObservers(x = x, se = se, input = input, session = session, pObjects = pObjects, rObjects = rObjects)

  expect_null(out)

  expect_named(rObjects, c(
    "PathwaysTable1_INTERNAL_table_update",
    "PathwaysTable1",
    "PathwaysTable1_INTERNAL_relinked_select",
    "PathwaysTable1_INTERNAL_saved_choices",
    "PathwaysTable1_INTERNAL_multi_select",
    "PathwaysTable1_INTERNAL_single_select"))
  for (i in names(rObjects)) {
    expect_type(rObjects[[i]], "integer")
  }
})

test_that(".defineDataInterface returns a list of UI elements", {
  x <- PathwaysTable()
  se <- SummarizedExperiment()

  out <- .defineDataInterface(x, se, select_info = NULL)

  expect_type(out, "list")
  for (i in seq_along(out)) {
    expect_s3_class(out[[i]], "shiny.tag")
  }
})

test_that(".defineInterface returns a list of shiny tag lists", {
  x <- PathwaysTable()
  se <- SummarizedExperiment()

  out <- .defineInterface(x, se, select_info = NULL)

  expect_type(out, "list")
  for (i in seq_along(out)) {
    expect_s3_class(out[[i]], "shiny.tag.list")
  }
})

test_that(".multiSelectionDimension return 'row'", {
  x <- PathwaysTable()

  out <- .multiSelectionDimension(x)

  expect_identical(out, "row")
})

test_that(".multiSelectionCommands returns a character vector", {
  x <- PathwaysTable()

  out <- .multiSelectionCommands(x)

  expect_type(out, "character")
})

test_that(".multiSelectionActive returns the current row selection", {
  x <- PathwaysTable()
  out <- .multiSelectionActive(x)
  expect_null(out)

  x <- PathwaysTable(Selected = "pathway_1")

  out <- .multiSelectionActive(x)
  expect_type(out, "character")
  expect_identical(out, "pathway_1")
})
