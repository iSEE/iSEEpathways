test_that(".embed_pathway_result", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- SummarizedExperiment()

  out <- iSEEpathways:::.embed_pathway_result(x, se, name = "result")

  expect_s4_class(metadata(out)$iSEEpathways$result, "iSEEfgseaResults")

  # repeat to trigger the warning about name already in use
  expect_warning(
    iSEEpathways:::.embed_pathway_result(x, out, name = "result"),
    "Results already exist under name"
  )
})

test_that("embedPathwaysResults works for data.frame", {
  se <- SummarizedExperiment()

  out <- embedPathwaysResults(fgsea_result, se, name = "result", pathwayType = "demo", class = "fgsea")

  expect_s4_class(metadata(out)$iSEEpathways$result, "iSEEfgseaResults")
})

test_that("embedPathwaysResults throw an error if class of result is not specified for data.frame", {
  se <- SummarizedExperiment()

  expect_error(
    embedPathwaysResults(fgsea_result, se, name = "result", pathwayType = "demo"),
    "must be a value in"
  )
})

test_that("embedPathwaysResults method works for iSEEfgseaResults", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- SummarizedExperiment()

  out <- embedPathwaysResults(x, se, name = "result")

  expect_s4_class(metadata(out)$iSEEpathways$result, "iSEEfgseaResults")
})

test_that("pathwaysResultsNames works", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- SummarizedExperiment()

  se <- embedPathwaysResults(x, se, name = "result")

  out <- pathwaysResultsNames(se)

  expect_identical(out, "result")
})

test_that("pathwaysResults works", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- SummarizedExperiment()

  se <- embedPathwaysResults(x, se, name = "result")

  out <- pathwaysResults(se)

  expect_type(out, "list")
  expect_named(out, "result")

  out <- pathwaysResults(se, "result")

  expect_s4_class(out, "iSEEfgseaResults")
})

test_that("pathwaysResults throws an error when the result name does not exist", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = pathways_list, featuresStats = gene_stats)
  se <- SummarizedExperiment()

  se <- embedPathwaysResults(x, se, name = "result")

  expect_error(
    pathwaysResults(se, "wrong"),
    "is not a valid pathway result name"
  )
})
