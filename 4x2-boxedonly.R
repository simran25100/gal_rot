# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# dev.off() 
# plotting above using base r for readability
# png("~/Documents/PhD/R/plots/full_data_mul-with-clusters.png",width = 16, height = 10, units = "in", res = 300)

# Define distance ranges
distance_ranges <- list(
  list(d_min = 0, d_max = 200, label = "2", ylim = c(-250, 200)),
  list(d_min = 200, d_max = 500, label = "5", ylim = c(-100, 70)),
  list(d_min = 500, d_max = 750, label = "7", ylim = c(-100, 70)),
  list(d_min = 750, d_max = 1000, label = "10", ylim = c(-50, 30)),
  list(d_min = 1000, d_max = 1200, label = "125", ylim = c(-50, 30)),
  list(d_min = 1200, d_max = 2000, label = "255", ylim = c(-50, 30))
)
cex = 1.9
pcex = 2
lwd = 3.5

# Set up 2x3 grid of plots
# Set up 2x3 grid of plots
par(mfrow = c(2, 3), mar = c(5, 5, 3, 2), bg = "white", 
    cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = FALSE)

for (i in seq_along(distance_ranges)) {
  range <- distance_ranges[[i]]
  label <- range$label
  d_min <- range$d_min
  d_max <- range$d_max
  ylim  <- range$ylim
  
  # Extract data and model results
  res_s <- resultss[[label]]  # Singh
  res_b <- resultsb[[label]]  # Bovy
  res_g <- resultsg[[label]]  # Glimpse
  res_k <- resultsk[[label]]  # KYSO
  
  data_s <- res_s$data  # GAIA data
  
  # Plot setup
  plot_title <- paste0(d_min, " < d < ", d_max, " pc, Mean Plx: ", round(res_s$mplx, 2))
  
  plot(0, 0, type = "n",
       xlab = expression(italic(l) ~ "(degrees)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       main = plot_title,
       xlim = c(360, 0),
       ylim = ylim,
       tck = -0.02,
       xaxs = "i", yaxs = "i"
  )
  
  axis(side = 3, labels = FALSE, tck = -0.02)
  axis(side = 4, labels = FALSE, tck = -0.02)
  
  # # Restrict points to plotting region
  # keep_points <- data_s$l >= 0 & data_s$l <= 360 & 
  #   data_s$pml*4.74047 >= ylim[1] & data_s$pml*4.74047 <= ylim[2]
  # data_plot <- data_s[keep_points, ]
  # 
  # # Plot 'Other' in grey
  # greycol = "#80808030"
  # points(data_plot$l, data_plot$pml*4.74047, 
  #        col = ifelse(data_plot$cluster == "Other", greycol, "transparent"), 
  #        pch = 16, cex = 1)
  
  # Plot the data points with 'Other' in grey
  points(data_s$l, data_s$pml * 4.74047, 
         col = ifelse(data_s$cluster == "Other", "#80808030", "transparent"), 
         pch = 16, cex = 1)
  
  # Plot clusters in color
  unique_clusters <- unique(data_plot$cluster[data_plot$cluster != "Other"])
  unique_names <- unique(clusters$name)
  n <- length(unique_names)
  colors <- paletteer_d("ggsci::default_ucscgb", n = n)
  cluster_colors <- setNames(colors, unique_names)
  pch_values <- 1:25
  cluster_pchs <- setNames(rep(pch_values, length.out = n), unique_names)
  
  for (cl_name in unique_clusters) {
    cluster_data <- data_plot[data_plot$cluster == cl_name, ]
    points(cluster_data$l, cluster_data$pml*4.74047, 
           col = cluster_colors[cl_name],
           pch = cluster_pchs[cl_name], 
           cex = 0.6)
  }
  
  # Restrict model lines to plotting region
  idx <- seq(1, length(res_s$l), by = 15)
  
  # Singh model
  keep_line <- res_s$l[idx] >= 0 & res_s$l[idx] <= 360 & 
    res_s$mul[idx] >= ylim[1] & res_s$mul[idx] <= ylim[2]
  lines(res_s$l[idx][keep_line], res_s$mul[idx][keep_line], col = "magenta", lwd = lwd, lty = 3)
  
  # Bovy model
  keep_line <- res_b$l[idx] >= 0 & res_b$l[idx] <= 360 & 
    res_b$mul[idx] >= ylim[1] & res_b$mul[idx] <= ylim[2]
  lines(res_b$l[idx][keep_line], res_b$mul[idx][keep_line], col = "blue", lwd = lwd, lty = 4)
  
  # lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = 3)  # Singh - dashed
  # lines(res_b$l, res_b$mul, col = "blue", lwd = lwd, lty = "44")     # Bovy - solid
  # lines(res_g$l, res_g$mul, col = "green", lwd = lwd, lty = 1)  # Glimpse - dotted
  # lines(res_k$l, res_k$mul, col = "red", lwd = lwd, lty = "aa")          # KYSO - dotdash
  
  # Grid
  # grid()
  
  
  M17 <- clusters[clusters$name == "M17", ]
  if (i == 6) {
    rect(
      xleft   = M17$l_min-4,
      xright  = M17$l_max+4,
      ybottom = M17$pml_min * 4.74047,
      ytop    = M17$pml_max * 4.74047,
      border  = cluster_colors["M17"],
      lwd     = 3,       lty     = 2
    )
  }
  
  IC405410417 <- clusters[clusters$name == "IC 405/410/417", ]
  if (i == 6) {
    rect(
      xleft   = IC405410417$l_min-2,
      xright  = IC405410417$l_max+2,
      ybottom = IC405410417$pml_min * 4.74047,
      ytop    = IC405410417$pml_max * 4.74047,
      border  = cluster_colors["IC 405/410/417"],
      lwd     = 2,       lty     = 2
    )
  }
  
  CanisMajor  <- clusters[clusters$name == "Canis Major OB1", ]
  if (i == 3) {
    rect(
      xleft   = CanisMajor $l_min-2,
      xright  = CanisMajor $l_max+2,
      ybottom = CanisMajor $pml_min * 4.74047,
      ytop    = CanisMajor $pml_max * 4.74047,
      border  = cluster_colors["Canis Major OB1"],
      lwd     = 2,       lty     = 2
    )
  }
  
  CanisMajor  <- clusters[clusters$name == "Canis Major OB1", ]
  if (i %in% 4:5) {
    rect(
      xleft   = CanisMajor $l_min-2,
      xright  = CanisMajor $l_max+2,
      ybottom = CanisMajor $pml_min * 4.74047 + 16,
      ytop    = CanisMajor $pml_max * 4.74047 - 13,
      border  = cluster_colors["Canis Major OB1"],
      lwd     = 2,       lty     = 2
    )
  }
  
  CanisMajor  <- clusters[clusters$name == "Canis Major OB1", ]
  if (i == 6) {
    rect(
      xleft   = CanisMajor $l_min-2,
      xright  = CanisMajor $l_max+2,
      ybottom = CanisMajor $pml_min * 4.74047 + -2,
      ytop    = CanisMajor $pml_max * 4.74047 - 6,
      border  = cluster_colors["Canis Major OB1"],
      lwd     = 2,       lty     = 2
    )
  }
  
  CygX  <- clusters[clusters$name == "Cyg X", ]
  if (i %in% 4:6) {
    rect(
      xleft   = CygX $l_min-2,
      xright  = CygX $l_max+2,
      ybottom = CygX $pml_min * 4.74047 + -2,
      ytop    = CygX $pml_max * 4.74047 + 1,
      border  = cluster_colors["Cyg X"],
      lwd     = 2,       lty     = 2
    )
  }
  
  IC1396 <- clusters[clusters$name == "IC 1396", ]
  if (i == 4) {
    rect(
      xleft   = IC1396$l_min-4,
      xright  = IC1396$l_max+4,
      ybottom = IC1396$pml_min * 4.74047,
      ytop    = IC1396$pml_max * 4.74047,
      border  = cluster_colors["IC 1396"],
      lwd     = 2,       lty     = 2
    )
  }
  
  NGC2264 <- clusters[clusters$name == "NGC 2264", ]
  if (i == 4) {
    rect(
      xleft   = NGC2264$l_min-4,
      xright  = NGC2264$l_max+4,
      ybottom = NGC2264$pml_min * 4.74047,
      ytop    = NGC2264$pml_max * 4.74047,
      border  = cluster_colors["NGC 2264"],
      lwd     = 2,       lty     = 2
    )
  }
  
  
  # Add legend to first plot only
  # if (i == 1) {
  # legend("topleft",
  #        legend = c("GAIA Data", "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model",unique_clusters),
  #        col = c("grey", "magenta", "navy", "green", "red", cluster_colors[unique_clusters]),
  #        lwd = c(NA, 2, 2, 2, 2, rep(NA, length(unique_clusters))),
  #        lty = c(NA, 3, 4, 1, 2, rep(NA, length(unique_clusters))),
  #        pch = c(16, NA, NA, NA, NA, cluster_pchs[unique_clusters]),
  #        bty = "n", cex=cex-0.4)
  # }

  legend("topleft",
         legend = c("GAIA Data", "This work Model", "Bovy Model", unique_clusters),
         col    = c("grey", "magenta", "navy", cluster_colors[unique_clusters]),
         lwd    = c(NA, 2, 2, rep(NA, length(unique_clusters))),
         lty    = c(NA, 3, 4, rep(NA, length(unique_clusters))),
         pch    = c(16, NA, NA, cluster_pchs[unique_clusters]),
         bty    = "n",
         cex    = cex - 0.4)
  
}    
# dev.off()
