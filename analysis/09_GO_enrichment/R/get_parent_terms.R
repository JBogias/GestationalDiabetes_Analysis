get_parent_terms <- function(module_list, ont = "BP") {
  
  require(dplyr)
  require(readr)
  require(here)
  
  modules <- names(module_list)
  modules <- modules[!modules %in% "grey"]
  
  if(ont == "BP") {
    for(i in modules) {
      tryCatch({
        reduced_terms_BP <- read_rds(
          here(paste0("data/GO_analysis/BP/", i, "_BP_reducedTerms.rds"))
        ) %>%
          dplyr::rename("GO_ID" = "go")
        
        parent_df <- module_list[[i]] %>%
          dplyr::filter(ontology == "BP") %>%
          left_join(reduced_terms_BP,
                    by = "GO_ID")
        
        write_csv(parent_df,
                  here(paste0("data/GO_analysis/parent_df/BP/",
                              i, "_BP_parent_df.csv")))
        
      }, error = function(e) {})
    }
  } else if (ont == "MF") {
    for(i in modules) {
      tryCatch({
        reduced_terms_MF <- read_rds(
          here(paste0("data/GO_analysis/MF/", i, "_MF_reducedTerms.rds"))
        ) %>%
          dplyr::rename("GO_ID" = "go")
        
        parent_df <- module_list[[i]] %>%
          dplyr::filter(ontology == "MF") %>%
          left_join(reduced_terms_MF,
                    by = "GO_ID")
        
        write_csv(parent_df,
                  here(paste0("data/GO_analysis/parent_df/MF/",
                              i, "_MF_parent_df.csv")))
        
      }, error = function(e) {})
    }
  } else if (ont == "CC") {
    for(i in modules) {
      tryCatch({
        reduced_terms_CC <- read_rds(
          here(paste0("data/GO_analysis/CC/", i, "_CC_reducedTerms.rds"))
        ) %>%
          dplyr::rename("GO_ID" = "go")
        
        parent_df <- module_list[[i]] %>%
          dplyr::filter(ontology == "CC") %>%
          left_join(reduced_terms_CC,
                    by = "GO_ID")
        
        write_csv(parent_df,
                  here(paste0("data/GO_analysis/parent_df/CC/",
                              i, "_CC_parent_df.csv")))
        
      }, error = function(e) {})
    }
  } else {
    print("Pick either BP, MF, or CC, for the ont argument.")
  }
  
}
