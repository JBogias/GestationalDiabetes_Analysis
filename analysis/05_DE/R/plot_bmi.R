plot_bmi <- function(metadata) {
  require(dplyr)
  require(ggplot2)
  require(magrittr)
  require(cowplot)
  
  bmi_legend <- cowplot::get_legend(
    metadata %>%
      ggplot() + 
      geom_bar(aes(x = BMI,
                   fill = Outcome)) + 
      scale_fill_manual(values = c("coral", "cornflowerblue")) +
      labs(fill = "Outcome:  ") +
      scale_x_continuous(limits = c(15, 55)) +
      scale_y_continuous(limits = c(0, 4)) +
      theme_bw() +
      theme(axis.title = element_text(colour = "black", size = 13),
            axis.text = element_text(colour = "black", size = 12),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.title = element_text(colour = "black", size = 14),
            legend.text = element_text(colour = "black", size = 12))
  )
  
  uncomp_bmi <- metadata %>%
    dplyr::filter(!Outcome == "GDM") %>%
    dplyr::select(BMI) %>%
    ggplot() + 
    geom_histogram(aes(x = BMI),
                   binwidth = 1,
                   colour = "darkblue", 
                   fill = "cornflowerblue") + 
    scale_x_continuous(limits = c(15, 55)) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_bw() +
    theme(axis.title.y = element_text(colour = "black", size = 15),
          axis.title.x = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = 12))
  
  gdm_bmi <- metadata %>%
    dplyr::filter(Outcome == "GDM") %>%
    dplyr::select(BMI) %>%
    ggplot() + 
    geom_histogram(aes(x = BMI),
                   binwidth = 1,
                   colour = "darkred", 
                   fill = "coral") + 
    scale_x_continuous(limits = c(15, 55)) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_bw() +
    theme(axis.title = element_text(colour = "black", size = 15),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = 12))
  
  return(cowplot::plot_grid(uncomp_bmi,
                     gdm_bmi,
                     bmi_legend,
                     nrow = 3,
                     rel_heights = c(1, 1, 0.1)))
}
