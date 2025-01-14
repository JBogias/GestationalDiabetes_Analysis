
plot_library_size <- function(dgelist) {
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(magrittr)
  require(forcats)
  require(edgeR)
  require(ggplot2)
  require(cowplot)
  
  non_norm <- dgelist$counts %>%
    colSums() %>%
    as.data.frame() %>%
    set_colnames("LibrarySize") %>%
    rownames_to_column(var = "Sample") %>%
    dplyr::arrange(desc(LibrarySize)) %>%
    mutate(Sample = fct_reorder(Sample, LibrarySize)) %>%
    ggplot() +
    geom_col(
      aes(x = Sample,
          y = LibrarySize),
      fill = "coral",
      colour = "brown"
    ) +
    labs(y = "Library Size (reads)",
         x = "Sample ID") +
    ggtitle("RNA-seq Library Sizes (Non-Normalised)") +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 10,
                                     angle = 45,
                                     vjust = 0.6),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 12),
          plot.title = element_text(colour = "black", size = 13, face = "bold"))
  
  norm <- dgelist %>%
    cpm(log = TRUE) %>%
    colSums() %>%
    as.data.frame() %>%
    set_colnames("LibrarySize") %>%
    rownames_to_column(var = "Sample") %>%
    dplyr::arrange(desc(LibrarySize)) %>%
    mutate(
      Sample = fct_reorder(Sample, LibrarySize)
    ) %>%
    ggplot() +
    geom_col(
      aes(x = Sample,
          y = LibrarySize),
      fill = "coral",
      colour = "brown"
    ) +
    labs(y = "Normalized Library Size (reads)",
         x = "Sample ID") +
    ggtitle("RNA-seq Library Sizes (TMM Normalised)") +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 10,
                                     angle = 45,
                                     vjust = 0.6),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 12),
          plot.title = element_text(colour = "black", size = 13, face = "bold"))
  
  return(cowplot::plot_grid(non_norm,
                            norm,
                            ncol = 2))
}
