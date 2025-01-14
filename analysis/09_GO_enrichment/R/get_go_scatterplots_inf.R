get_go_scatterplots_inf <- function(module_list) {
  
  require(dplyr)
  require(tidyr)
  require(tibble)
  require(stringr)
  require(readr)
  require(magrittr)
  require(ggplot2)
  require(ggrepel)
  require(magrittr)
  require(rrvgo)
  require(here)
  
  modules <- names(module_list)
  modules <- modules[!modules %in% "grey"]
  
    suppressWarnings(
      for(i in modules) {
      tryCatch({
        # Biological Processes
        sim_matrix_BP <- calculateSimMatrix(module_list[[i]]$GO_ID,
                                            orgdb = "org.Hs.eg.db",
                                            ont = "BP",
                                            method = "Rel")
        
        sim_matrix_BP %>% 
          write_rds(here(paste0("data/GO_analysis/inf/BP/", i, "_BP_simMatrix.rds")))
        
        scores_BP <- setNames(-log10(module_list[[i]]$FDR), module_list[[i]]$GO_ID)
        reduced_terms_BP <- reduceSimMatrix(sim_matrix_BP,
                                            scores_BP,
                                            threshold = 0.9,
                                            orgdb = "org.Hs.eg.db")
        
        reduced_terms_BP %>%
          write_rds(
            here(paste0("data/GO_analysis/inf/BP/", i, "_BP_reducedTerms.rds"))
          )
        
        # Molecular Function
        sim_matrix_MF <- calculateSimMatrix(module_list[[i]]$GO_ID,
                                            orgdb = "org.Hs.eg.db",
                                            ont = "MF",
                                            method = "Rel")
        
        sim_matrix_MF %>% 
          write_rds(here(paste0("data/GO_analysis/inf/MF/", i, "_MF_simMatrix.rds")))
        
        scores_MF <- setNames(-log10(module_list[[i]]$FDR), module_list[[i]]$GO_ID)
        reduced_terms_MF <- reduceSimMatrix(sim_matrix_MF,
                                            scores_MF,
                                            threshold = 0.9,
                                            orgdb = "org.Hs.eg.db")
        
        reduced_terms_MF %>%
          write_rds(
            here(paste0("data/GO_analysis/inf/MF/", i, "_MF_reducedTerms.rds"))
          )
        
        # Cellular Component
        sim_matrix_CC <- calculateSimMatrix(module_list[[i]]$GO_ID,
                                            orgdb = "org.Hs.eg.db",
                                            ont = "CC",
                                            method = "Rel")
        
        sim_matrix_CC %>% 
          write_rds(here(paste0("data/GO_analysis/inf/CC/", i, "_CC_simMatrix.rds")))
        
        scores_CC <- setNames(-log10(module_list[[i]]$FDR), module_list[[i]]$GO_ID)
        reduced_terms_CC <- reduceSimMatrix(sim_matrix_CC,
                                            scores_CC,
                                            threshold = 0.9,
                                            orgdb = "org.Hs.eg.db")
        
        reduced_terms_CC %>%
          write_rds(
            here(paste0("data/GO_analysis/inf/CC/", i, "_CC_reducedTerms.rds"))
          )
      }, error = function(e) {})
      }
    )
    
    suppressWarnings(
      for(i in modules) {
      tryCatch({
        sim_matrix_BP <- read_rds(
          here(paste0("data/GO_analysis/inf/BP/", i, "_BP_simMatrix.rds"))
        )
        
        reduced_terms_BP <- read_rds(
          here(paste0("data/GO_analysis/inf/BP/", i, "_BP_reducedTerms.rds"))
        )
        
        scatterGO_BP <- scatterPlot(sim_matrix_BP,
                                    reduced_terms_BP,
                                    labelSize = 3.5) +
          ggtitle(paste0(str_to_title(i), " Module - Biological Processes")) +
          theme_linedraw() +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank())
        
        ggsave(filename = paste0(i, "GO_scatterplot_BP.png"),
               plot = scatterGO_BP,
               device = "png",
               path = here("figures/GO_scatterplots/inf/BP/"),
               width = 2700,
               height = 2500,
               units = "px",
               dpi = 400)
        
        sim_matrix_MF <- read_rds(
          here(paste0("data/GO_analysis/inf/MF/", i, "_MF_simMatrix.rds"))
        )
        
        reduced_terms_MF <- read_rds(
          here(paste0("data/GO_analysis/inf/MF/", i, "_MF_reducedTerms.rds"))
        )
        
        scatterGO_MF <- scatterPlot(sim_matrix_MF,
                                    reduced_terms_MF,
                                    labelSize = 3.5) +
          ggtitle(paste0(str_to_title(i), " Module - Molecular Functions")) +
          theme_linedraw() +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank())
        
        ggsave(filename = paste0(i, "GO_scatterplot_MF.png"),
               plot = scatterGO_MF,
               device = "png",
               path = here("figures/GO_scatterplots/inf/MF/"),
               width = 2700,
               height = 2500,
               units = "px",
               dpi = 400)
        
        sim_matrix_CC <- read_rds(
          here(paste0("data/GO_analysis/inf/CC/", i, "_CC_simMatrix.rds"))
        )
        
        reduced_terms_CC <- read_rds(
          here(paste0("data/GO_analysis/inf/CC/", i, "_CC_reducedTerms.rds"))
        )
        
        scatterGO_CC <- scatterPlot(sim_matrix_CC,
                                    reduced_terms_CC,
                                    labelSize = 3.5) +
          ggtitle(paste0(str_to_title(i), " Module - Cellular Components")) +
          theme_linedraw() +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank())
        
        ggsave(filename = paste0(i, "GO_scatterplot_CC.png"),
               plot = scatterGO_CC,
               device = "png",
               path = here("figures/GO_scatterplots/inf/CC/"),
               width = 2700,
               height = 2500,
               units = "px",
               dpi = 400)
      }, error = function(e) {})
      }
    )
}
