joined_scatterplot <- function(modules, ont) {
  
  require(dplyr)
  require(tidyr)
  require(tibble)
  require(stringr)
  require(readr)
  require(magrittr)
  require(ggplot2)
  require(ggrepel)
  require(cowplot)
  require(here)
  
  scatterGO_BP <- vector("list", 9) %>% set_names(modules)
  
  inner_modules <- c("blue", "brown", "greenyellow", "pink")
  y_modules <- c("black", "green")
  x_modules <- c("salmon", "turquoise")
  xy_module <- "purple"
  
  for(i in inner_modules) {
    tryCatch({sim_matrix_BP <- read_rds(
      here(paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_simMatrix.rds"))
    )
    
    reduced_terms_BP <- read_rds(
      here(
        paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_reducedTerms.rds")
        )
    )
    
    scatterGO_BP[[i]] <- scatterplot_better(sim_matrix_BP,
                                            reduced_terms_BP,
                                            size = "score",
                                            labelSize = 6) +
      ggtitle(paste0(str_to_title(i), " Module - Biological Processes")) +
      theme(plot.title = element_text(colour = "black", size = 17),
            axis.ticks = element_blank())
    }, error = function(e) {})
  }
  
  for(i in y_modules) {
    tryCatch({sim_matrix_BP <- read_rds(
      here(paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_simMatrix.rds"))
    )
    
    reduced_terms_BP <- read_rds(
      here(
        paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_reducedTerms.rds")
        )
    )
    
    scatterGO_BP[[i]] <- scatterplot_better(sim_matrix_BP,
                                            reduced_terms_BP,
                                            size = "score",
                                            labelSize = 6) +
      ggtitle(paste0(str_to_title(i), " Module - Biological Processes")) +
      theme(plot.title = element_text(colour = "black", size = 17),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.x = element_blank())
    }, error = function(e) {})
  }
  
  for(i in x_modules) {
    tryCatch({sim_matrix_BP <- read_rds(
      here(paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_simMatrix.rds"))
    )
    
    reduced_terms_BP <- read_rds(
      here(
        paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_reducedTerms.rds")
        )
    )
    
    scatterGO_BP[[i]] <- scatterplot_better(sim_matrix_BP,
                                            reduced_terms_BP,
                                            size = "score",
                                            labelSize = 6) +
      ggtitle(paste0(str_to_title(i), " Module - Biological Processes")) +
      theme(plot.title = element_text(colour = "black", size = 17),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.ticks.y = element_blank())
    }, error = function(e) {})
  }
  
  for(i in xy_module) {
    tryCatch({sim_matrix_BP <- read_rds(
      here(paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_simMatrix.rds"))
    )
    
    reduced_terms_BP <- read_rds(
      here(
        paste0("data/GO_analysis/", ont, "/", i, "_", ont, "_reducedTerms.rds")
        )
    )
    
    scatterGO_BP[[i]] <- scatterplot_better(sim_matrix_BP,
                                            reduced_terms_BP,
                                            size = "score",
                                            labelSize = 6) +
      ggtitle(paste0(str_to_title(i), " Module - Biological Processes")) +
      theme(plot.title = element_text(colour = "black", size = 17),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 14))
    }, error = function(e) {})
  }
  
  scatter_cowplot <- cowplot::plot_grid(scatterGO_BP$black,
                                        scatterGO_BP$blue,
                                        scatterGO_BP$brown,
                                        scatterGO_BP$green,
                                        scatterGO_BP$greenyellow,
                                        scatterGO_BP$pink,
                                        scatterGO_BP$purple,
                                        scatterGO_BP$salmon,
                                        scatterGO_BP$turquoise,
                                        ncol = 3,
                                        nrow = 3,
                                        labels = c("A", "B", "C",
                                                   "D", "E", "F",
                                                   "G", "H", "I"))
  
  x_title <- cowplot::ggdraw() +
    draw_label(
      paste0("Principal Coordinate 1"),
      colour = "black",
      size = 20
    )
  
  y_title <- cowplot::ggdraw() +
    draw_label(
      paste0("Principal Coordinate 2"),
      colour = "black",
      size = 20,
      angle = 90
    )
  
  blank <- ggplot + theme_void()
  
  scatter_cowplot <- cowplot::plot_grid(y_title,
                                        scatter_cowplot,
                                        blank,
                                        x_title,
                                        ncol = 2,
                                        nrow = 2,
                                        rel_widths = c(0.05, 1),
                                        rel_heights = c(1, 0.05))
  
}
