make_dgelist <- function(counts, metadata, annotation,
                         minCPM = 1, minSamp = 23) {
  require(dplyr)
  require(tibble)
  require(magrittr)
  require(edgeR)
  
  gene_anno <- annotation %>%
    dplyr::filter(type == "gene") %>%
    dplyr::select(-transcript_id, 
                  -transcript_name,
                  -type) %>%
    mutate(
      region_key = paste0(
        chromosome, ":", start, "-", end, ":", strand
      )
    )
  
  keepTheseGenes <- (rowSums(cpm(counts) >= minCPM) > minSamp)
  
  dgelist <- counts %>%
    DGEList(
      samples = colnames(.) %>%
        enframe(name = NULL,
                value = "ID") %>%
        left_join(metadata),
      genes = rownames(.) %>%
        enframe(name = NULL,
                value = "gene_id") %>%
        left_join(gene_anno,
                  by = c("gene_id")) %>%
        dplyr::select("gene_id",
                      "gene_name",
                      "gene_biotype",
                      "chromosome")
    ) %>%
    magrittr::extract(keepTheseGenes, ) %>%
    calcNormFactors(method = "TMM")
  
  return(dgelist)
}
