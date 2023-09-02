#' Simulate Example Data
#'
#' @param n_pathways integer scalar, number of pathways to simulate.
#' @param n_features integer scalar, number of features to simulate.
#' @param pathway_sizes integer vector, possible sizes of pathway to sample from.
#' @param seed a single value, passed to [set.seed()].
#'
#' @return A list of two elements:
#' \describe{
#' \item{pathwaysList}{A named list of dummy pathways.
#' Names represent pathway identifiers;
#' values represent character vectors of feature identifiers.}
#' }
#' \item{featuresStat}{A named numeric vector.
#' Names represent feature identifiers;
#' values represent 'scores' (e.g., log2 fold-change)}
#' @export
#'
#' @examples
#' simulated_data <- simulateExampleData()
#' head(lengths(simulated_data$pathwaysList))
#' head(simulated_data$featuresStat)
simulateExampleData <- function(n_pathways = 5E3, n_features = 15E3, pathway_sizes = 16:500, seed = 1) {
  gene_ids <- paste0("feature_", seq_len(n_features))
  pathway_ids <- paste0("pathway_", seq_len(n_pathways))
  set.seed(seed)
  pathway_lengths <- sample(pathway_sizes, length(pathway_ids), replace = TRUE)
  names(pathway_lengths) <- pathway_ids
  set.seed(seed)
  pathways_list <- lapply(pathway_ids, function(x) {sample(gene_ids, pathway_lengths[[x]])})
  names(pathways_list) <- pathway_ids
  set.seed(seed)
  gene_stats <- rnorm(length(gene_ids))
  names(gene_stats) <- gene_ids
  return(list(
    pathwaysList = pathways_list,
    featuresStat = gene_stats
  ))
}