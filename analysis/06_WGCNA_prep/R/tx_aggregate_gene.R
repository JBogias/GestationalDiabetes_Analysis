tx_aggregate_gene <- function(counts, anno) {
  require(dplyr)
  require(magrittr)
  
  gene_counts <- counts %>%
    left_join(
      anno %>%
        dplyr::select(
          "gene_id",
          "transcript_id"
        ),
      by = "transcript_id"
    ) %>%
    dplyr::select(-"transcript_id") %>%
    group_by(gene_id) %>%
    summarise_if(
      is.numeric,
      sum
    ) %>%
    dplyr::filter(
      gene_id %in% unique(anno$gene_id)
    ) %>%
    column_to_rownames("gene_id") %>%
    as.matrix()
  
  return(gene_counts)
}