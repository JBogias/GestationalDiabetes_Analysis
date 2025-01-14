#Remade scatterplot function from rrvgo but made it able to limit the number 
# of lables shown. So only the labels with the top 5 highest scores are shown. 
# I also changed the theme to `theme_linedraw()`

scatterplot_better <- function(simMatrix,
                               reducedTerms,
                               size = "score",
                               addLabel = TRUE, 
                               labelSize = 3) {
  require(dplyr)
  require(ggplot2)
  require(magrittr)
  require(ggrepel)
  require(here)
  
  if (!all(sapply(c("ggplot2", "ggrepel"), requireNamespace, 
                  quietly = TRUE))) {
    stop("Packages ggplot2, ggrepel and/or its dependencies not available. ", 
         "Consider installing them before using this function.", 
         call. = FALSE)
  }
  x <- cmdscale(as.matrix(as.dist(1 - simMatrix)),
                eig = TRUE, 
                k = 2)
  df <- cbind(as.data.frame(x$points),
              reducedTerms[
                match(rownames(x$points), reducedTerms$go),
                c("term", "parent", "parentTerm", "size")
              ]
  )
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = V1,
                 y = V2,
                 color = parentTerm)) + 
    ggplot2::geom_point(ggplot2::aes(size = size),
                        alpha = 0.5) + 
    ggplot2::scale_color_discrete(guide = "none") +
    ggplot2::scale_size_continuous(guide = "none", 
                                   range = c(4, 25)) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), name = "") + 
    ggplot2::scale_y_continuous(limits = c(-1, 1), name = "") +
    ggplot2::theme_linedraw() + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_blank())
  if (addLabel) {
    p + ggrepel::geom_label_repel(
      data = tail(
        dplyr::arrange(subset(df, parent == rownames(df)), size), 5
      ),
      aes(label = parentTerm), 
      box.padding = grid::unit(1, "lines"),
      size = labelSize,
      force = 3
    )
  }
  else {
    p
  }}