
# This is a HIGHLY specific function that I have defined here solely to keep my
# code within the markdown tidy. Do not use for anything else.

create_table_one <- function(metadata) {
  require(tidyverse)
  require(magrittr)
  uncomplicated_table <- reframe(
    pd %>%
      dplyr::filter(Outcome == "Uncomplicated") %>%
      group_by(FetalSex),
    "FetalSex" = length(FetalSex),
    "BMI" = c(
      round(mean(BMI), 1), round(range(BMI), 1)
    ),
    "MaternalAge" = c(
      round(mean(Age), 1), round(range(Age), 1)
    ),
    "GestationalAge" = c(
      round(mean(GestationalAge), 1), round(range(GestationalAge), 1)
    ),
    "Ethnicity_percent" = round(
      divide_by(
        sum(Ethnicity_Simplified == "Caucasian"),
        length(Ethnicity_Simplified)
      )*100, 1
    ),
    "Metabolic_Syndrome" = round(
      divide_by(
        sum(metabolicSyndrome == "Yes"), 
        length(metabolicSyndrome)
      )*100, 1
    ),
    "Birthweight" = c(
      round(mean(Birthweight), 1), round(range(Birthweight), 1))
  )
  
  uncomplicated_table <- uncomplicated_table %>% 
    as.data.frame() %>%
    set_rownames(c("F_mean", "F_min", "F_max",
                   "M_mean", "M_min", "M_max"))
  
  uncomp_tbl <- uncomplicated_table %>%
    t() %>%
    as.data.frame() %>%
    mutate(Female = paste0(F_mean, " (", F_min, "-", F_max, ")"),
           Male = paste0(M_mean, " (", M_min, "-", M_max, ")"))
  
  
  gdm_table <- reframe(
    pd %>%
      dplyr::filter(Outcome == "GDM") %>%
      group_by(FetalSex),
    "FetalSex" = length(FetalSex),
    "BMI" = c(
      round(mean(BMI), 1), round(range(BMI), 1)
    ),
    "MaternalAge" = c(
      round(mean(Age), 1), round(range(Age), 1)
    ),
    "GestationalAge" = c(
      round(mean(GestationalAge), 1), round(range(GestationalAge), 1)
    ),
    "Ethnicity_percent" = round(
      divide_by(
        sum(Ethnicity_Simplified == "Caucasian"),
        length(Ethnicity_Simplified)
      )*100, 1
    ),
    "Metabolic_Syndrome" = round(
      divide_by(
        sum(metabolicSyndrome == "Yes"), 
        length(metabolicSyndrome)
      )*100, 1
    ),
    "Birthweight" = c(
      round(mean(Birthweight), 1), round(range(Birthweight), 1))
  )
  
  gdm_table <- gdm_table %>% 
    as.data.frame() %>%
    set_rownames(c("F_mean", "F_min", "F_max",
                   "M_mean", "M_min", "M_max"))
  
  gdm_table <- gdm_table %>%
    t() %>%
    as.data.frame() %>%
    mutate(Female = paste0(F_mean, " (", F_min, "-", F_max, ")"),
           Male = paste0(M_mean, " (", M_min, "-", M_max, ")"))
  
  patient_table <- uncomp_tbl %>%
    dplyr::select("Female_U" = Female,
                  "Male_U" = Male) %>%
    rownames_to_column("id") %>%
    left_join(gdm_table %>%
                dplyr::select("Female_G" = Female,
                              "Male_G" = Male) %>%
                rownames_to_column("id"),
              by = "id") %>%
    dplyr::select("Patient_Characteristics" = id,
                  "Female_Uncomplicated" = Female_U,
                  "Female_GDM" = Female_G,
                  "Male_Uncomplicated" = Male_U,
                  "Male_GDM" = Male_G)
  
  patient_table[1, ] <- patient_table[1, ] %>%
    lapply(function(x) {
      str_replace(pattern = " \\s*\\([^\\)]+\\)", replacement = "", string = x)
    })
  
  return(patient_table)
}
