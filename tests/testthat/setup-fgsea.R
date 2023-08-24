stopifnot(requireNamespace("fgsea"))

gene_ids <- paste0("gene_", 1:100)
pathway_ids <- paste0("pathway_", 1:5)
set.seed(1)
pathway_lengths <- sample(10:20, length(pathway_ids), replace = TRUE)
names(pathway_lengths) <- pathway_ids
set.seed(1)
pathways_list <- lapply(pathway_ids, function(x) {sample(gene_ids, pathway_lengths[[x]])})
names(pathways_list) <- pathway_ids
set.seed(1)
gene_stats <- rnorm(length(gene_ids))
names(gene_stats) <- gene_ids
set.seed(1)
fgsea_result <- fgsea::fgsea(pathways = pathways_list,
  stats    = gene_stats,
  minSize  = 10,
  maxSize  = 500)
