test_that("simulateExampleData produces the expected pieces of data", {
  
  out <- simulateExampleData()
  
  expect_named(out, c("pathwaysList", "featuresStat", "summarizedexperiment"))
  expect_type(out$pathwaysList, "list")
  expect_type(out$featuresStat, "double")
  expect_s4_class(out$summarizedexperiment, "SummarizedExperiment")
})