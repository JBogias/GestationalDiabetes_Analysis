plot_highest_exp <- function(dgelist, metadata, annotation) {
  require(dplyr)
  require(tidyr)
  require(tibble)
  require(matrixStats)
  require(magrittr)
  require(ggplot2)
  require(edgeR)
  require(cowplot)
  
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
  
  gene_df <- dgelist %>%
    cpm(log = TRUE) %>%
    as.data.frame()
  
  uncomp_samples <- metadata %>%
    dplyr::filter(Outcome == "Uncomplicated") %>%
    dplyr::select(ID) %>%
    as.matrix() %>%
    as.character() 
  
  uncomp_order <- gene_df %>%
    dplyr::select(all_of(uncomp_samples)) %>%
    as.matrix() %>%
    matrixStats::rowMedians() %>% 
    set_names(rownames(gene_df)) %>%
    as.data.frame() %>%
    set_colnames("median") %>%
    dplyr::arrange(desc(median)) %>%
    head(20) %>%
    rownames()
  
  uncomp_df <- gene_df %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(cols = colnames(gene_df),
                 names_to = "ID",
                 values_to = "log2CPM") %>%
    dplyr::filter(ID %in% uncomp_samples) %>%
    dplyr::filter(gene_id %in% uncomp_order) %>%
    as.data.frame() %>%
    inner_join(gene_anno %>%
                 dplyr::select(gene_id,
                               gene_name,
                               gene_biotype),
               by = "gene_id") %>%
    dplyr::mutate(gene_id = factor(gene_id, levels = uncomp_order)) %>%
    as_tibble()
  
  gdm_order <- gene_df %>%
    dplyr::select(-all_of(uncomp_samples)) %>%
    as.matrix() %>%
    matrixStats::rowMedians() %>% 
    set_names(rownames(gene_df)) %>%
    as.data.frame() %>%
    set_colnames("median") %>%
    dplyr::arrange(desc(median)) %>%
    head(20) %>%
    rownames()
  
  gdm_df <- gene_df %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(cols = colnames(gene_df),
                 names_to = "ID",
                 values_to = "log2CPM") %>%
    dplyr::filter(!ID %in% uncomp_samples) %>%
    dplyr::filter(gene_id %in% gdm_order) %>%
    as.data.frame() %>%
    inner_join(gene_anno %>%
                 dplyr::select(gene_id,
                               gene_name,
                               gene_biotype),
               by = "gene_id") %>%
    dplyr::mutate(
      gene_id = factor(gene_id, levels = gdm_order)) %>%
    as_tibble()
  
  uncomp_plot_l <- uncomp_df %>%
    dplyr::arrange(desc(log2CPM)) %>%
    mutate(gene_name = fct_reorder(gene_name, log2CPM, .desc = TRUE),
           gene_biotype = str_replace(gene_biotype,
                                      "protein_coding",
                                      "Protein Coding"),
           gene_biotype = str_replace(gene_biotype,
                                      "pseudogene",
                                      "Pseudogene")) %>%
    ggplot() +
    geom_boxplot(
      aes(x = gene_name,
          y = log2CPM,
          fill = gene_biotype)
    ) +
    labs(x = "",
         y = "Gene Expression (Log2 CPM)",
         fill = "Gene Biotype") +
    ggtitle("Highest expressed genes in uncomplicated placenta") +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 9,
                                     #angle = 45,
                                     vjust = 0.6),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 12),
          plot.title = element_text(colour = "black",
                                    size = 13,
                                    face = "bold"))
  
  legend_expr <- get_legend(uncomp_plot_l)
  
  uncomp_plot <- uncomp_plot_l +
    theme(legend.position = "none")
  
  gdm_plot <- gdm_df %>%
    dplyr::arrange(desc(log2CPM)) %>%
    mutate(gene_name = fct_reorder(gene_name, log2CPM, .desc = TRUE),
           gene_biotype = str_replace(gene_biotype,
                                      "protein_coding",
                                      "Protein Coding"),
           gene_biotype = str_replace(gene_biotype,
                                      "pseudogene",
                                      "Pseudogene")) %>%
    ggplot() +
    geom_boxplot(
      aes(x = gene_name,
          y = log2CPM,
          fill = gene_biotype)
    ) +
    labs(x = "",
         y = "Gene Expression (Log2 CPM)",
         fill = "Gene Biotype") +
    ggtitle("Highest expressed genes in GDM placenta") +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(colour = "black",
                                     size = 9,
                                     #angle = 45,
                                     vjust = 0.6),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 12),
          plot.title = element_text(colour = "black", size = 13, face = "bold"))
  
  expr_cowplot <- cowplot::plot_grid(uncomp_plot,
                                     gdm_plot,
                                     ncol = 1,
                                     labels = c("a", "b"))
  
  # Save at 900x900 pixels
  return(cowplot::plot_grid(expr_cowplot,
                            legend_expr,
                            ncol = 2,
                            rel_widths = c(1, 0.15)))
}