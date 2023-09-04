stopifnot(requireNamespace("fgsea"))

setup_data <- simulateExampleData(n_pathways = 5, n_features = 100, pathway_sizes = 10:20, seed = 1)

fgsea_result <- fgsea::fgsea(
  pathways = setup_data$pathwaysList,
  stats    = setup_data$featuresStat,
  minSize  = 10,
  maxSize  = 500)
