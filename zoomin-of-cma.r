source("~/Documents/PhD/R/calculate_proper_motion_l.R")
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(data.table)

dfcma <- read.csv("/Users/ssingh/Documents/PhD/R/cma_all_full.CSV")

plot_cluster_zoom_cma <- function(cluster_names,
                                  cluster_colors,
                                  save_plot = FALSE,
                                  outdir = "/Users/ssingh/Documents/PhD/R/plots/zoomins/") {
  
  # Create output directory if saving
  if (save_plot && !dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Open PNG device if saving
  if (save_plot) {
    file_name <- paste0(outdir, paste(cluster_names, collapse="_"), "_zoom.png")
    png(file_name, width = 1500, height = 1200, res = 200)
  }
  
  # Adjust margins: leave space on the right for legend
  par(mfrow = c(1,1), mar = c(4, 4, 1, 5), oma = c(0,0,0,4.5)) # mar = bottom, left, top, right
  cex <- 1.0
  
  # Select cluster stars
  cluster_stars <- dfcma[dfcma$cluster_name %in% cluster_names, ]
  if(nrow(cluster_stars) == 0) stop("No stars found for selected clusters.")
  
  # Define distance bins and symbols
  dist_breaks <- c(500, 900, 1200, 2000)
  dist_pch    <- c(0, 1, 2)
  
  # Assign pch based on distance
  dfcma$pch_dist <- as.numeric(as.character(
    cut(dfcma$D, breaks = dist_breaks, labels = dist_pch, include.lowest = TRUE)
  ))
  
  # Base plot: all other stars
  other_stars <- dfcma[!dfcma$cluster_name %in% cluster_names, ]
  plot(other_stars$l, other_stars$pml * 4.74047,
       col = "#80808030",
       pch = 18,
       cex = cex,
       xlab = expression(italic(l) ~ "(deg)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       xlim = c(max(cluster_stars$l) + 1.5,
                min(cluster_stars$l) - 0.5),
       ylim = c(min(cluster_stars$pml * 4.74047) ,
                max(cluster_stars$pml * 4.74047) ),
       main = NA,
       xaxs = "r", yaxs = "r")
  
  cluster_colors <- c(
    "CanisMajor0"  = "#E41A1C",  # red
    "CanisMajor1"  = "#377EB8",  # blue
    "CanisMajor2"  = "#FF7F00",  # orange
    "CanisMajor6"  = "#4DAF4A",  # green
    "CanisMajor8"  = "#984EA3",  # purple
    "CanisMajor9"  = "#F781BF",  # pink
    "CanisMajor17" = "#A65628",  # brown
    "CanisMajor18" = "#EFC000"   # gold
  )
  
  # Plot each cluster
  for(cl in cluster_names) {
    sub <- dfcma[dfcma$cluster_name == cl, ]
    points(sub$l, sub$pml * 4.74047,
           pch = sub$pch_dist,
           col = cluster_colors[cl],
           cex = cex)
  }
  

  for (i in seq_along(distance_ranges)) {
  range <- distance_ranges[[i]]
  label <- range$label
  res_s <- resultss[[label]]  # Singh
  res_b <- resultsb[[label]]  # Bovy

  if (i == 5) {
  # Add model lines
  lines(res_s$l, res_s$mul, col = "magenta", lwd = 4, lty = 3, xpd = FALSE)
  lines(res_b$l, res_b$mul, col = "blue", lwd = 4, lty = 4, xpd = FALSE)
  lines(res_c$l[idx], res_c$mul[idx], col = "green", lwd = lwd, lty = 3, xpd = FALSE)   # Singh Circ - dotted
  
  }}
  
  par(xpd = NA)  # allow drawing outside plot region
  
  # --- Cluster legend outside on the right ---
  usr <- par("usr")  # get plot coordinates
  clusternames <- c("Alessi 21", "PHOC 4", "[HXH2021] 32", "VdB 92", "CanisMajor8",
                    "NGC 2318 ", "NGC 2323 ", "NGC 2343")
  legend(
    x = usr[2] + 0.05 * (usr[2] - usr[1]),  # slightly to the right of plot
    y = usr[4],                             # align with top
    legend = c(clusternames, "GAIA Data",
               "This Work (Non-Circ Model)", "Bovy Model", "This Work (Circ Model)"),
    col = c(cluster_colors[cluster_names], "#80808080",
            "magenta", "blue", "green"),
    pch = c(rep(16, length(cluster_names)), 18, NA, NA, NA),
    lty = c(rep(NA, length(cluster_names) + 1), 3, 4, 1),
    lwd = c(rep(NA, length(cluster_names) + 1), 4, 4, 4),
    bty = "n",
    cex = 1,
    yjust = 1
  )
  
  
  # --- Distance legend inside plot (bottom right) ---
  legend(x = usr[2] + 0.05*(usr[2]-usr[1]),  # slightly right of plot
         y = usr[3],  # 
         legend = c("500-900 pc", "900-1300 pc", "1300-2000 pc"),
         pch = dist_pch,
         col = "black",
         bty = "n",
         cex = 1)
  
  # Close device if saving
  if(save_plot) {
    dev.off()
    message("Saved plot: ", file_name)
  }
}


canis_clusters <- c("CanisMajor0", "CanisMajor1", "CanisMajor2", "CanisMajor6",
                    "CanisMajor8", "CanisMajor9", "CanisMajor17", "CanisMajor18")

plot_cluster_zoom_cma(cluster_names = canis_clusters,
                      cluster_colors = cluster_colors)

