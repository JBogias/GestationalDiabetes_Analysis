plot_module_sizes <- function(mergedColours, go_list_modules) {
  require(dplyr)
  require(readr)
  require(magrittr)
  require(ggplot2)
  require(cowplot)
  require(here)
  
  plot_module_sizes <- mergedColours %>%
    table() %>%
    as.data.frame() %>%
    set_colnames(c("module", "genes")) %>%
    dplyr::filter(!module == "grey") %>%
    dplyr::arrange(desc(genes)) %>%
    left_join(go_list_modules %>%
                dplyr::select("module",
                              "term",
                              "FDR"),
              by = "module") %>%
    mutate(module = factor(module))
  
  size <- plot_module_sizes %>%
    ggplot(
      aes(y = reorder(x = module,
                      X = genes),
          x = genes,
          fill = module)
    ) + 
    geom_bar(stat = "identity") +
    labs(x = "Genes",
         y = "Modules") +
    scale_fill_manual(values = levels(plot_module_sizes$module)) +
    ggtitle("Modules by size") +
    guides(fill = "none") +
    theme_bw() +
    theme(plot.title = element_text(size = 14,
                                    colour = "black",
                                    face = "bold"),
          axis.title = element_text(size = 12,
                                    colour = "black"),
          axis.text = element_text(size = 10,
                                   colour = "black"))
  
  go_labels <- ggplot() +
    geom_label_repel(data = plot_module_sizes,
                     aes(x = 1000,
                         y = reorder(x = module,
                                     X = genes),
                         fill = FDR,
                         label = term),
                     nudge_x = -10,
                     segment.colour = "transparent",
                     box.padding = 0
    ) +
    scale_fill_gradient(low = "lightgreen",
                        high = "pink") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm")) 
  
  go_labels_legend <- ggplot() +
    geom_label_repel(data = plot_module_sizes,
                     aes(x = 1000,
                         y = reorder(x = module,
                                     X = genes),
                         fill = -log10(FDR),
                         label = term),
                     nudge_x = -10,
                     segment.colour = "transparent",
                     box.padding = 0) +
    scale_fill_gradientn(
      colours = c("pink", "lightyellow", "lightgreen"),
      limits = c(1, 35),
      breaks = c(1, 9, 18, 27, 35),
      labels = c("0.1", "", "1e-18", "", "1e-35")
    ) +
    labs(fill = "Enrichment FDR") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  blanc <- ggplot() +
    geom_blank() +
    theme_void()
  
  label_column <- plot_grid(blanc,
                            go_labels,
                            blanc,
                            nrow = 3,
                            ncol = 1,
                            rel_heights = c(0.08, 1, 0.1))
  
  go_legend <- cowplot::get_legend(go_labels_legend)
  
  GO_module_sizes <- cowplot::plot_grid(size,
                                        label_column,
                                        nrow = 1,
                                        ncol = 2)
  
  plot_module_sizes %>%
    dplyr::select("module",
                  "genes") %>%
    write_csv(here("data/supp_table1.csv"))
  
  return(GO_module_sizes)
}
