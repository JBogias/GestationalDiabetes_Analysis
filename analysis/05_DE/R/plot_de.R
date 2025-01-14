plot_de <- function(de_genes, dgelist, type = "tile") {
  
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(magrittr)
  require(edgeR)
  require(cowplot)
  require(ggplot2)
  
  sig_genes <- de_genes %>% 
  dplyr::filter(FDR < 0.05 & abs(logFC) > 1) %>%
  dplyr::select(gene_name) %>%
  as.matrix() %>%
  as.character()

sig_genes <- de_genes %>% 
  dplyr::filter(
    FDR < 0.05 & abs(logFC) > 1
  ) %>%
  dplyr::select(
    gene_id,
    gene_name
  ) %>%
  as.data.frame() %>%
  dplyr::filter(
    !gene_name %in% "hsa-mir-6723"
  )

de_plot <- dgelist %>%
  cpm(log = TRUE) %>%
  as.data.frame() %>%
  rownames_to_column("gene_id") %>%
  dplyr::filter(
    gene_id %in% sig_genes$gene_id
  ) %>%
  pivot_longer(
    cols = colnames(dgelist),
    names_to = "ID",
    values_to = "log2CPM"
  ) %>%
  left_join(
    de_genes %>%
      dplyr::select("gene_id", "gene_name", "FDR"),
    by = "gene_id"
  ) %>%
  left_join(
    dgelist$samples,
    by = "ID"
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(x = log2CPM,
        y = factor(gene_name, levels = rev(sig_genes$gene_name)),
        fill = factor(Outcome, levels = c("Uncomplicated", "GDM"))),
    colour = "black"
  ) +
  scale_fill_manual(
    values = c("blue", "darkorange")
  ) +
  labs(x = "Gene expression (log2 CPM)",
       y = "",
       fill = "Pregnancy Outcome") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      size = 16, colour = "black", face = "bold"
    ),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "grey95"),
    axis.line = element_line(colour = "black", linewidth = 1),
    axis.title = element_text(
      size = 16, colour = "black", face = "bold"
    ),
    axis.text = element_text(
      size = 14, colour = "black"
    ),
    legend.title = element_text(
      size = 18, colour = "black", face = "bold"
    ),
    legend.text = element_text(
      size = 16, colour = "black"
    ),
    plot.margin = unit(c(0.5, -0.49, 0.5, 0.5), "cm")
  )

if (type == "tile") {
  sig_plot <- gdm_sig %>% 
    ggplot() + 
    geom_tile(aes(x = DE,
                  y = factor(gene_name, levels = rev(sig_genes$gene_name)),
                  fill = FDR)) +
    guides(fill = guide_legend(reverse = FALSE)) +
    scale_fill_gradient(low = "green", high = "yellow") +
    geom_text(aes(x = DE,
                  y = factor(gene_name, levels = rev(sig_genes$gene_name)),
                  label = FDR %>%
                    formatC(digits = 2, format = "e"))) +
    #ggtitle("Good-bye old friend, and may the Force be with you") +
    labs(x = "FDR",
         y = "") +
    theme_minimal() +
    theme(line = element_blank(),
          plot.title = element_text(
            size = 14, colour = "transparent", face = "bold"
          ),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "transparent"),
          axis.title.x = element_text(
            size = 16, colour = "black"
          ),
          plot.margin = unit(c(0.5, 0.5, 0.5, -0.49), "cm")) +
    theme(legend.position = "none")
  
} else if (type == "bar") {
  
  sig_plot <- gdm_sig %>% 
    ggplot() + 
    geom_col(aes(x = -log10(FDR),
                 y = factor(gene_name, levels = rev(sig_genes$gene_name)),
                 fill = as.numeric(FDR))) +
    guides(fill = guide_legend(reverse = FALSE)) +
    scale_fill_gradient(low = "green", high = "yellow", limits = c(1.35e-9, 0.05),
                        breaks = c(0.001, 0.01, 0.02, 0.03, 0.045),
                        labels = c("< 0.001", "0.01", "0.02", "0.03", "0.04")) +
    scale_x_continuous(breaks = c(0, 8)) +
    #ggtitle("Good-bye old friend, and may the Force be with you") +
    labs(x = "FDR (-log10)",
         y = "",
         fill = "FDR") +
    theme_minimal() +
    theme(line = element_blank(),
          plot.title = element_text(
            size = 14, colour = "transparent", face = "bold"
          ),
          axis.line.x = element_line(colour = "black", linewidth = 1),
          axis.ticks.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black", linewidth = 1),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.title.x = element_text(
            size = 16, colour = "black", face = "bold"
          ),
          legend.title = element_text(colour = "black", size = 14,
                                      face = "bold"),
          legend.text = element_text(colour = "black", size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, -0.49), "cm"))
}

leg_plot <- cowplot::get_legend(de_plot)
de_cowplot <- de_plot + theme(legend.position = "none")

top_row <- cowplot::plot_grid(de_cowplot, sig_plot,
                              nrow = 1,
                              rel_widths = c(1, 0.5))

return(cowplot::plot_grid(top_row,
                          leg_plot,
                          ncol = 1,
                          rel_heights = c(1, 0.1)))
}
