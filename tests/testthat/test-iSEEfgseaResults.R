# library(testthat); library(iSEEpathways)

test_that("iSEEfgseaResults constructor works", {
  out <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)
  expect_s4_class(out, "iSEEfgseaResults")
})

test_that("iSEEpathwaysResults validity method catches errors", {
  expect_error(
    iSEEfgseaResults(data = fgsea_result, pathwayType = NA_integer_, pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat),
    "pathwayType(.Object) must be a character scalar", fixed = TRUE
  )

  expect_error(
    iSEEfgseaResults(data = fgsea_result, pathwayType = c("demo", "testthat"), pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat),
    "pathwayType(.Object) must be length 1", fixed = TRUE
  )
})

test_that("iSEEpathwaysResults getters work", {
  x <- iSEEfgseaResults(data = fgsea_result, pathwayType = "demo", pathwaysList = setup_data$pathwaysList, featuresStats = setup_data$featuresStat)

  expect_identical(pathwayType(x), "demo")
  expect_identical(pathwaysList(x), setup_data$pathwaysList)
  expect_identical(featuresStats(x), setup_data$featuresStat)
})
