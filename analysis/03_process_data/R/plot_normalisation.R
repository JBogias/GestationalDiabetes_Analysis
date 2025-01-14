plot_normalisation <- function(dgelist) {
  require(dplyr)
  require(tibble)
  require(reshape2)
  require(ggplot2)
  require(edgeR)
  require(cowplot)
  
  dgelist_noNorm <- dgelist
  dgelist_noNorm$samples$norm.factors <- 1
  
  non_norm <- dgelist_noNorm %>% 
    cpm(log = TRUE) %>%
    melt() %>%
    mutate(Var2 = as.character(Var2)) %>%
    ggplot(
      aes(group = Var2,
          y = value,
          fill = Var2)
    ) + 
    geom_boxplot(colour = "black") +
    ggtitle("Non-Normalised Gene Counts") +
    labs(y = "Non-Normalised Gene Counts (log2 CPM)") +
    guides(fill = "none") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, colour = "black", face = "bold"),
      axis.title = element_text(size = 12, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  norm <- dgelist %>% 
    cpm(log = TRUE) %>%
    melt() %>%
    mutate(Var2 = as.character(Var2)) %>%
    ggplot(
      aes(group = Var2,
          y = value,
          fill = Var2)
    ) + 
    geom_boxplot(colour = "black") +
    ggtitle("Normalised Gene Counts") +
    labs(x = "Samples",
         y = "Normalised Gene Counts (log2 CPM)") +
    guides(fill = "none") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, colour = "black", face = "bold"),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  return(cowplot::plot_grid(
    non_norm,
    norm,
    nrow = 2,
    ncol = 1
  ))
}
