#' Simulate Example Data
#'
#' @description
#' Simulates various pieces of data for the purpose of demonstration in vignettes
#' and help pages.
#'
#' @details
#' At least for the time being, this function generates dummy data
#' purely for the purpose of demonstrating the *format* of expected inputs.
#'
#' As such, the independent pieces of simulated data are just that -- independent --
#' in the meaning that simulated counts, statistics, and pathways are not related
#' numerically, and do not make any biological sense.
#'
#' The only coherent piece of information is the set of feature identifiers,
#' carefully coordinated between the rownames of the count matrix,
#' the names of the feature statistics,
#' and the set of features in the list of pathways,
#' so that panels in the app can transmit and interpret that shared piece of information.
#'
#' @param n_pathways integer scalar, number of pathways to simulate.
#' @param n_features integer scalar, number of features to simulate.
#' @param n_samples integer scalar, number of samples to simulate.
#' @param pathway_sizes integer vector, possible sizes of pathway to sample from.
#'
#' @return A list of three elements:
#' \describe{
#' \item{pathwaysList}{A named list of dummy pathways.
#' Names represent pathway identifiers;
#' values represent character vectors of feature identifiers.}
#' \item{featuresStat}{A named numeric vector of dummy feature-wise statistics.
#' Names represent feature identifiers;
#' values represent 'scores' (e.g., log2 fold-change)}
#' \item{summarizedexperiment}{A [SummarizedExperiment-class] object
#' that contains a count matrix with rownames and colnames.}
#' }
#' @export
#' @importFrom stats rnbinom rnorm
#' @importFrom SummarizedExperiment SummarizedExperiment
#'
#' @examples
#' set.seed(1)
#' simulated_data <- simulateExampleData()
#' head(lengths(simulated_data$pathwaysList))
#' head(simulated_data$featuresStat)
simulateExampleData <- function(n_pathways = 5E3, n_features = 15E3, n_samples = 8, pathway_sizes = 15:500) {
  # identifiers
  feature_ids <- paste0("feature_", seq_len(n_features))
  pathway_ids <- paste0("pathway_", seq_len(n_pathways))
  sample_ids <- paste0("sample_", seq_len(n_samples))
  # pathways
  pathway_lengths <- sample(pathway_sizes, length(pathway_ids), replace = TRUE)
  names(pathway_lengths) <- pathway_ids
  pathways_list <- lapply(pathway_ids, function(x) { sample(feature_ids, pathway_lengths[[x]]) })
  names(pathways_list) <- pathway_ids
  # features
  feature_stats <- rnorm(length(feature_ids))
  names(feature_stats) <- feature_ids
  # samples
  count_matrix <- matrix(rnbinom(n=n_features*n_samples, mu=100, size=1/0.5), nrow=n_features, dimnames = list(feature_ids, sample_ids))
  se_object <- SummarizedExperiment(assays = list(counts = count_matrix))
  return(list(
    pathwaysList = pathways_list,
    featuresStat = feature_stats,
    summarizedexperiment = se_object
  ))
}
