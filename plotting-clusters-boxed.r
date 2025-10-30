source("~/Documents/PhD/R/calculate_proper_motion_l.R")
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(data.table)
library(paletteer) # for the colours


file_path <- '/Users/ssingh/Documents/PhD/cat_unique_ss/gaia_211025_lb-pmlb-nueff-plxbias.csv'
dfs <- fread(file_path)       # gaia (singh) data frame

file_path <- "~/Documents/PhD/cluster/glimpse-kyso-sscu/glimpse/glimpse.csv"
dfg <- fread(file_path)       # glimpse data frame

file_path <- "~/Documents/PhD/cluster/glimpse-kyso-sscu/kyso/kyso.csv"
dfk <- fread(file_path)       # kyso data frame

# Define gaia conditions
cond1_s <- (dfs$b > -30) & (dfs$b < 30)
cond2_s <- dfs$plx_corr > 0.5         #< (1/0.75)
cond3_s <- dfs$amax < 0.5
cond4_s <- dfs$epsi <= 1
cond5_s <- dfs$RUWE <= 1.4

# Define glimpse conditions
cond1_g <- (dfg$b > -30) & (dfg$b < 30)
cond2_g <- dfg$plx_corr > 0.5         #< (1/0.75)
cond3_g <- dfg$amax < 0.5
cond4_g <- dfg$epsi <= 1
cond5_g <- dfg$RUWE <= 1.4

# Define kyso conditions
cond1_k <- (dfk$b > -30) & (dfk$b < 30)
cond2_k <- dfk$plx_corr > 0.5
cond3_k <- dfk$amax < 0.5
cond4_k <- dfk$epsi <= 1
cond5_k <- dfk$RUWE <= 1.4


# My clusters file 
clusters <- read.csv("/Users/ssingh/Documents/PhD/R/clusters.csv", stringsAsFactors = FALSE)
cluster_colors <- setNames(clusters$colour, clusters$name)

dfs$cluster <- "Other"
for (i in seq_len(nrow(clusters))) {
  cl <- clusters[i, ]
  
  # Longitude check 
  if (cl$l_min > cl$l_max) {
    spatial_mask <- ((dfs$l >= cl$l_min & dfs$l <= 360) | (dfs$l >= 0 & dfs$l <= cl$l_max))
  } else {
    spatial_mask <- dfs$l >= cl$l_min & dfs$l <= cl$l_max
  }
  
  # latitude filter
  spatial_mask <- spatial_mask & dfs$b >= cl$b_min & dfs$b <= cl$b_max
  
  # apply d and plx error filters to stars in spatial box, excl NAs
  in_region <- spatial_mask &
    !is.na(dfs$distance) & dfs$distance >= cl$d_min & dfs$distance <= cl$d_max &
    !is.na(dfs$e_Plx) & dfs$e_Plx <= cl$plx_err_max &
    !is.na(dfs$pml) & dfs$pml >= cl$pml_min & dfs$pml <= cl$pml_max 
  
  # Step 5: Assign cluster name to the stars that meet the conditions
  dfs$cluster[in_region] <- cl$name
  
  # Optional: print summary for debugging
  cat(cl$name, ": filtered to", sum(in_region), "stars\n")
}

# Define distance ranges in parsecs
dist_ranges_s <- list(
  "2"  = list(cond = (dfs$dist_corr >= 0)    & (dfs$dist_corr < 200), ylim = c(-170, 150)),
  "5"  = list(cond = (dfs$dist_corr >= 200)  & (dfs$dist_corr < 500), ylim = c(-80, 55)),
  "7"  = list(cond = (dfs$dist_corr >= 500)  & (dfs$dist_corr < 750), ylim = c(-70, 40)),
  "10" = list(cond = (dfs$dist_corr >= 750)  & (dfs$dist_corr < 1000), ylim = c(-50, 30)),
  "125" = list(cond = (dfs$dist_corr >= 1000) & (dfs$dist_corr < 1200), ylim = c(-50, 30)),
  "255" = list(cond = (dfs$dist_corr >= 1200) & (dfs$dist_corr < 2000), ylim = c(-50, 30))
)

# Define distance ranges in parsecs
dist_ranges_g <- list(
  "2"  = list(cond = (dfg$dist_corr >= 0)    & (dfg$dist_corr < 200), ylim = c(-170, 150)),
  "5"  = list(cond = (dfg$dist_corr >= 200)  & (dfg$dist_corr < 500), ylim = c(-80, 55)),
  "7"  = list(cond = (dfg$dist_corr >= 500)  & (dfg$dist_corr < 750), ylim = c(-70, 40)),
  "10" = list(cond = (dfg$dist_corr >= 750)  & (dfg$dist_corr < 1000), ylim = c(-50, 30)),
  "125" = list(cond = (dfg$dist_corr >= 1000) & (dfg$dist_corr < 1200), ylim = c(-50, 30)),
  "255" = list(cond = (dfg$dist_corr >= 1200) & (dfg$dist_corr < 2000), ylim = c(-50, 30))
)

# Define distance ranges in parsecs
dist_ranges_k <- list(
  "2"  = list(cond = (dfk$dist_corr >= 0)    & (dfk$dist_corr < 200), ylim = c(-170, 150)),
  "5"  = list(cond = (dfk$dist_corr >= 200)  & (dfk$dist_corr < 500), ylim = c(-80, 55)),
  "7"  = list(cond = (dfk$dist_corr >= 500)  & (dfk$dist_corr < 750), ylim = c(-70, 40)),
  "10" = list(cond = (dfk$dist_corr >= 750)  & (dfk$dist_corr < 1000), ylim = c(-50, 30)),
  "125" = list(cond = (dfk$dist_corr >= 1000) & (dfk$dist_corr < 1200), ylim = c(-50, 30)),
  "255" = list(cond = (dfk$dist_corr >= 1200) & (dfk$dist_corr < 2000), ylim = c(-50, 30))
)


# Singh (GAIA best-fit)
A_s  <- 15.14332
B_s  <- -14.5146
C_s  <- 2.576765
K_s  <- 11.18119
u0_s <- 9.882971
v0_s <- 10.62474
w0_s <- 7.111708

# Bovy + Reid
A_b  <- 15.3
B_b  <- -11.9
C_b  <- -3.2
K_b  <- -3.3
u0_b <- 10.6
v0_b <- 10.7
w0_b <- 7.6

# Singh Circ params
A_c <- 15.08682 
B_c <- -14.3864 
u0_c <- 8.985908 
v0_c <- 10.74527 
w0_c <- 6.750529 

# Glimpse (using Singh parameters)
A_g  <- 15.93285
B_g  <- -13.14542
C_g  <- 2.301406
K_g  <- -26.37416
u0_g <- 5.946431
v0_g <- 12.50922
w0_g <- 6.825491

# KYSO model
A_k  <- 8.679173
B_k  <- -19.33214
C_k  <- 0.2601185
K_k  <- 24.10391
u0_k <- 10.52325
v0_k <- 13.38128
w0_k <- 7.651548



build_model_results <- function(data, conds, dist_ranges, A, B, C, K, u0, v0, w0) {
  results <- list()
  for (label in names(dist_ranges)) {
    dist_cond <- dist_ranges[[label]]$cond
    filtered <- data[conds & dist_cond, ]
    mplx <- mean(filtered$plx_corr, na.rm = TRUE)
    pml <- filtered$pml
    l <- seq(0, 360, length.out = nrow(filtered))
    b <- rep(5, length(l))
    pm <- calculate_proper_motion_l(A, B, C, K, l, b, mplx, u0, v0, w0)
    idx <- order(l)
    results[[label]] <- list(
      data = filtered,
      l = l[idx],
      b = b[idx],
      mul = pm$mul[idx],
      mub = pm$mub[idx],
      mplx = mplx,
      pml = pml[idx],
      ylim = dist_ranges[[label]]$ylim
    )
  }
  return(results)
}

resultss <- build_model_results(dfs, cond2_s & cond3_s & cond4_s & cond5_s, dist_ranges_s, A_s, B_s, C_s, K_s, u0_s, v0_s, w0_s)
resultsb <- build_model_results(dfs, cond2_s & cond3_s & cond4_s & cond5_s, dist_ranges_s, A_b, B_b, C_b, K_b, u0_b, v0_b, w0_b)
resultsg <- build_model_results(dfg, cond2_g & cond3_g & cond4_g & cond5_g, dist_ranges_g, A_g, B_g, C_g, K_g, u0_g, v0_g, w0_g)
resultsk <- build_model_results(dfk, cond2_k & cond3_k & cond4_k & cond5_k, dist_ranges_k, A_k, B_k, C_k, K_k, u0_k, v0_k, w0_k)
resultsc <- build_model_results(dfs, cond2_s & cond3_s & cond4_s & cond5_s, dist_ranges_s, A_c, B_c, 0, 0, u0_c, v0_c, w0_c)



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
par(mfrow = c(2, 3), mar = c(5, 5, 3, 2), bg = "white", cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = FALSE)

for (i in seq_along(distance_ranges)) {
  range <- distance_ranges[[i]]
  label <- range$label
  d_min <- range$d_min
  d_max <- range$d_max
  ylim  <- range$ylim
  
  # Extract data and model results
  res_s <- resultss[[label]]  # Singh
  res_b <- resultsb[[label]]  # Bovy
  res_c <- resultsc[[label]]  # Circ
  
  data_s <- res_s$data  # GAIA data
  
  # Plot setup
  plot_title <- paste0(d_min, " < d < ", d_max, " pc, Mean Plx: ", round(res_s$mplx, 2))
  
  plot(data_s$l, data_s$pml * 4.74047, type = "n",
       xlab = expression(italic(l) ~ "(degrees)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       main = plot_title,
       xlim = c(360, 0),
       ylim = ylim,
       tck = -0.02,  # Tick marks on all sides
       xaxs = "r", yaxs = "r"
  )
  
  axis(side = 3, labels = FALSE, tck = -0.02)
  axis(side = 4, labels = FALSE, tck = -0.02)
  
  # Get unique clusters (excluding "Other")
  unique_clusters <- unique(data_s$cluster[data_s$cluster != "Other"])  # Exclude "Other"
  
  # Generate random colors and pch values for each cluster
  set.seed(123)
  # cluster_colors <- if (length(unique_clusters) > 0) setNames(adjustcolor(rainbow(length(unique_clusters)), alpha.f = 0.4), unique_clusters) else character(0)
  cluster_pchs <- setNames(sample(1:25, length(unique_clusters), replace = TRUE), unique_clusters) 
  
  # Plot the data points with 'Other' in grey
  points(data_s$l, data_s$pml * 4.74047, 
         col = ifelse(data_s$cluster == "Other", "#80808030", "transparent"), 
         pch = 16, cex = 1)
   
  
  # Plot clusters in color
  unique_clusters <- unique(data_s$cluster[data_s$cluster != "Other"])
  unique_names <- unique(clusters$name)
  n <- length(unique_names)
  # colors <- rev(as.character(paletteer_d("ggthemes::Hue_Circle", n = n)))
  colors <- rev(rainbow(n, start = 0, end = 1))  # full 360° color wheel
  cluster_colors <- setNames(colors, unique_names)
  pch_values <- 1:25
  cluster_pchs <- setNames(rep(pch_values, length.out = n), unique_names)
  
  for (cl_name in unique_clusters) {
    cluster_data <- data_s[data_s$cluster == cl_name, ]
    points(cluster_data$l, cluster_data$pml*4.74047, 
           col = cluster_colors[cl_name],
           pch = cluster_pchs[cl_name], 
           cex = 0.6)
  }
  
  idx <- seq(1, length(res_s$l), by = 15) # every 10th point 
  # Add model lines
  lines(res_s$l[idx], res_s$mul[idx], col = "magenta", lwd = lwd, lty = 1, xpd = FALSE)   # Singh NC - dashed
  lines(res_b$l[idx], res_b$mul[idx], col = "blue", lwd = lwd, lty = 4, xpd = FALSE)     # Bovy - solid
  lines(res_c$l[idx], res_c$mul[idx], col = "green", lwd = lwd, lty = 3, xpd = FALSE)   # Singh Circ - dotted
  
  # Grid
  grid()
  
  orion <- clusters[clusters$name == "Orion", ]
  if (i == 2) {
    rect(
      xleft   = orion$l_min-2,
      xright  = orion$l_max+2,
      ybottom = orion$pml_min * 4.74047,
      ytop    = orion$pml_max * 4.74047,
      border  = cluster_colors["Orion"],
      lwd     = 2
    )
  }
  
  M17 <- clusters[clusters$name == "M17", ]
  if (i == 6) {
    rect(
      xleft   = M17$l_min-2,
      xright  = M17$l_max+2,
      ybottom = M17$pml_min * 4.74047,
      ytop    = M17$pml_max * 4.74047,
      border  = cluster_colors["M17"],
      lwd     = 2
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
      lwd     = 2
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
      lwd     = 2
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
      lwd     = 2
    )
  }
  # Add legend to first plot only
  # if (i == 1) {
  legend("topleft",
         legend = c("GAIA Data", "This work N-Circ Model", "Bovy Model", "This work Circ Model", unique_clusters),
         col = c("grey", "magenta", "navy", "green", cluster_colors[unique_clusters]),
         lwd = c(NA, 2, 2, 2, rep(NA, length(unique_clusters))),
         lty = c(NA, 3, 4, 1, rep(NA, length(unique_clusters))),
         pch = c(16, NA, NA, NA, cluster_pchs[unique_clusters]),
         bty = "n",   cex = cex - 0.4)
  # }
}    
# dev.off()