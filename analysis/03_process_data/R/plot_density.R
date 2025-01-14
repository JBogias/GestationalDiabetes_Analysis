filterCounts <- function(x, minCPM = 2, minSamp = 23) {
  require(edgeR)
  cpm <- cpm(x)
  i <- rowSums(cpm > minCPM) > minSamp
  return(x[i,])
}

plot_density <- function(gene_counts) {
  require(dplyr)
  require(tibble)
  require(magrittr)
  require(edgeR)
  require(RColorBrewer)
  require(here)
  
  lcpm <- cpm(
    gene_counts,
    log = TRUE
  )
  
  plot.new()
  nsamples <- ncol(gene_counts)
  col <- brewer.pal(nsamples, "Paired")
  
  par(mfrow = c(1, 2))
  plot(density(lcpm[ , 1]),
       col = col[1],
       lwd = 2,
       ylim = c(0, 0.26),
       las = 2,
       main = "",
       xlab = "")
  title(main = "A. Raw data",
        xlab = "Log-cpm")
  
  for (i in 2:nsamples) {
    den <- density(lcpm[ , i])
    lines(den$x,
          den$y,
          col = col[i],
          lwd = 2)
  }
  
  filtered_counts <- gene_counts %>%
    filterCounts(minCPM = 1, minSamp = 23)
  
  lcpm <- cpm(filtered_counts,
              log = TRUE)
  plot(density(lcpm[ , 1]),
       col = col[1],
       lwd = 2,
       ylim = c(0, 0.26),
       las = 2,
       main = "",
       xlab = "")
  
  title(main = "B. Filtered data",
        xlab = "Log-cpm")
  
  for (i in 2:nsamples) {
    den <- density(lcpm[ , i])
    lines(den$x,
          den$y,
          col = col[i],
          lwd = 2)
  }
}
