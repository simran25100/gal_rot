source("~/Documents/PhD/R/calculate_proper_motion_l.R")
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(data.table)

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
A_s  <- 15.246
B_s  <- -14.17035
C_s  <- 2.990482
K_s  <- 7.093847
u0_s <- 8.940881
v0_s <- 11.58836
w0_s <- 6.92108


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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
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
  res_g <- resultsg[[label]]  # Glimpse
  res_k <- resultsk[[label]]  # KYSO
  
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
  
  # Plot each cluster in a different color (except "Other")
  for (cl_name in unique_clusters) {
    cluster_data <- data_s[data_s$cluster == cl_name, ]
    points(cluster_data$l, cluster_data$pml * 4.74047, 
           col = cluster_colors[cl_name],  
           pch = cluster_pchs[cl_name], 
           cex = 0.85)
  }
  # Add model lines
  lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = "1342")  # Singh - dashed
  lines(res_b$l, res_b$mul, col = "blue", lwd = lwd, lty = 4)     # Bovy - solid
  lines(res_g$l, res_g$mul, col = "green", lwd = lwd, lty = 1)  # Glimpse - dotted
  lines(res_k$l, res_k$mul, col = "red", lwd = lwd, lty = "aa")          # KYSO - dotdash
  
  # Grid
  # grid()
  
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
           legend = c("GAIA Data", "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model",unique_clusters),
           col = c("grey", "magenta", "navy", "green", "red", cluster_colors[unique_clusters]),
           lwd = c(NA, 2, 2, 2, 2, rep(NA, length(unique_clusters))),
           lty = c(NA, 3, 4, 1, 2, rep(NA, length(unique_clusters))),
           pch = c(16, NA, NA, NA, NA, cluster_pchs[unique_clusters]),
           bty = "n", cex=cex-0.4)
  # }
}    
# dev.off()
plot.new()  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Zoom in regoin
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
# Orion Zoom-in
par(mfrow = c(1,1), xpd = FALSE)
range <- distance_ranges[[2]]
label <- range$label

# Extract data and model results
res_s <- resultss[[label]]  # Singh
res_b <- resultsb[[label]]  # Bovy
res_g <- resultsg[[label]]  # Glimpse
res_k <- resultsk[[label]]  # KYSO

data_s <- res_s$data  # GAIA data

par(mfrow = c(1,1), xpd = FALSE)
orion_stars <- dfs[grepl("Orion", dfs$cluster, ignore.case = TRUE), ]

plot(orion_stars$l, orion_stars$pml * 4.74047,
     xlab = expression(italic(l) ~ "(deg)"),
     ylab = expression(mu[l] ~ "(km/s/kpc)"),
     main = "Orion Zoom-In",
     xlim = c(min(orion_stars$l)-1, max(orion_stars$l)+1),
     ylim = c(min(orion_stars$pml*4.74047)-5, max(orion_stars$pml*4.74047)+5),
     pch = 16, col = cluster_colors["Orion"], cex = 1.2,
     xaxs = "r", yaxs = "r")
grid()

axis(side = 3, labels = FALSE, tck = -0.02)
axis(side = 4, labels = FALSE, tck = -0.02)

# Get unique clusters (excluding "Other")
unique_clusters <- unique(data_s$cluster[data_s$cluster != "Other"])  # Exclude "Other"

# Generate random colors and pch values for each cluster
set.seed(123)
cluster_colors <- cluster_colors["Orion"]

# Add model lines
lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = "1342")  # Singh - dashed
lines(res_b$l, res_b$mul, col = "blue", lwd = lwd, lty = 4)     # Bovy - solid
lines(res_g$l, res_g$mul, col = "green", lwd = lwd, lty = 1)  # Glimpse - dotted
lines(res_k$l, res_k$mul, col = "red", lwd = lwd, lty = "aa")          # KYSO - dotdash

# Add legend to first plot only
# if (i == 1) {
legend("topleft",
       legend = c("GAIA Data", "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model"),
       col = c(cluster_colors, "magenta", "navy", "green", "red" ),
       lwd = c(NA, 2, 2, 2, 2),
       lty = c(NA, 3, 4, 1, 2),
       pch = c(16, NA, NA, NA, NA),
       bty = "n", cex=cex-0.4)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reset to single plot
par(mfrow = c(1,1), xpd = FALSE)

range <- distance_ranges[[6]]
label <- range$label

# Extract data and model results
res_s <- resultss[[label]]  # Singh
res_b <- resultsb[[label]]  # Bovy
res_g <- resultsg[[label]]  # Glimpse
res_k <- resultsk[[label]]  # KYSO

data_s <- res_s$data  # GAIA data

# Select M17 stars
M17_stars <- dfs[grepl("M17", dfs$cluster, ignore.case = TRUE), ]

plot(M17_stars$l, M17_stars$pml * 4.74047,
     xlab = expression(italic(l) ~ "(deg)"),
     ylab = expression(mu[l] ~ "(km/s/kpc)"),
     main = "M17 Zoom-In",
     xlim = c(min(M17_stars$l) - 1, max(M17_stars$l) + 1),
     ylim = c(min(M17_stars$pml * 4.74047) - 5,
              max(M17_stars$pml * 4.74047) + 5),
     pch = 16, col = cluster_colors["M17"], cex = 1.2,
     xaxs = "r", yaxs = "r")

grid()

axis(side = 3, labels = FALSE, tck = -0.02)
axis(side = 4, labels = FALSE, tck = -0.02)

unique_clusters <- unique(data_s$cluster[data_s$cluster != "Other"])  # Exclude "Other"
cluster_colors <- cluster_colors["M17"]

# Add model lines
lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = 3)  # Singh
lines(res_b$l, res_b$mul, col = "blue",    lwd = lwd, lty = 4)  # Bovy
lines(res_g$l, res_g$mul, col = "green",   lwd = lwd, lty = 1)  # Glimpse
lines(res_k$l, res_k$mul, col = "red",     lwd = lwd, lty = 2)  # KYSO

# Legend
legend("topleft",
       legend = c("M17", "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model"),
       col = c(cluster_colors["M17"], "magenta", "blue", "green", "red"),
       lwd = c(NA, 2, 2, 2, 2),
       lty = c(NA, 3, 4, 1, 2),
       pch = c(16, NA, NA, NA, NA),
       bty = "n", cex = cex - 0.4)


  # dev.off()
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Zoom in regoin
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Function to plot a cluster zoom-in
plot_cluster_zoom <- function(cluster_name, dist_label, cluster_colors) {
  # Reset plot
  par(mfrow = c(1,1), xpd = FALSE)
  
  # Extract distance range results
  res_s <- resultss[[dist_label]]  # Singh
  res_b <- resultsb[[dist_label]]  # Bovy
  res_g <- resultsg[[dist_label]]  # Glimpse
  res_k <- resultsk[[dist_label]]  # KYSO
  data_s <- res_s$data  # GAIA data
  
  # Select cluster stars (flexible grepl)
  cluster_stars <- dfs[grepl(gsub("[-– ]", "[-– ]", cluster_name), dfs$cluster, ignore.case = TRUE), ]
  if(nrow(cluster_stars) == 0) stop(paste("No stars found for", cluster_name))
  
  # Plot main cluster stars
  plot(cluster_stars$l, cluster_stars$pml * 4.74047,
       xlab = expression(italic(l) ~ "(deg)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       main = paste(cluster_name, "Zoom-In"),
       xlim = c(max(cluster_stars$l) + 0.5, min(cluster_stars$l) - 0.5),
       ylim = c(min(cluster_stars$pml * 4.74047) - 1, max(cluster_stars$pml * 4.74047) + 1),
       pch = 16, col = cluster_colors[cluster_name], cex = 1.2,
       xaxs = "r", yaxs = "r")
  
  # Add model lines
  lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = 3)  # Singh
  lines(res_b$l, res_b$mul, col = "blue",    lwd = lwd, lty = 4)  # Bovy
  lines(res_g$l, res_g$mul, col = "green",   lwd = lwd, lty = 1)  # Glimpse
  lines(res_k$l, res_k$mul, col = "red",     lwd = lwd, lty = 2)  # KYSO
  
  # Grid and axes
  grid()
  axis(side = 3, labels = FALSE, tck = -0.02)
  axis(side = 4, labels = FALSE, tck = -0.02)
  
  # Add legend
  legend("topleft",
         legend = c(cluster_name, "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model"),
         col = c(cluster_colors[cluster_name], "magenta", "blue", "green", "red"),
         lwd = c(NA, 2, 2, 2, 2),
         lty = c(NA, 3, 4, 1, 2),
         pch = c(16, NA, NA, NA, NA),
         bty = "n", cex = cex - 0.4)
}

# Set cluster colors
set.seed(123)
cluster_colors <- setNames(adjustcolor(rainbow(5), alpha.f = 0.4), c("Orion", "M17", "IC 405/410/417"))

# Plot zoom-ins
plot_cluster_zoom("Orion", dist_label = "5", cluster_colors)
plot_cluster_zoom("M17",  dist_label =   "255", cluster_colors)
plot_cluster_zoom("IC 405/410/417",dist_label =  "255", cluster_colors)





plot_cluster_zoom <- function(cluster_name,
                              dist_label,
                              cluster_colors,
                              legend_pos = NULL,   # <-- new argument
                              save_plot = TRUE,
                              outdir   = "/Users/ssingh/Documents/PhD/R/plots/zoomins/") { 
  # Create output directory if saving
  if (save_plot && !dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  # File name for saving
  if (save_plot) {
    file_name <- paste0(outdir, gsub("[[:space:]/]", "_", cluster_name), "_zoom.png")
    png(file_name, width = 1200, height = 1200, res = 200)
  }
  
  # Reset plot
  par(mfrow = c(1,1), xpd = FALSE)
  cex=1.2
  
  # Extract distance range results
  res_s <- resultss[[dist_label]]  # Singh
  res_b <- resultsb[[dist_label]]  # Bovy
  res_g <- resultsg[[dist_label]]  # Glimpse
  res_k <- resultsk[[dist_label]]  # KYSO
  data_s <- res_s$data  # GAIA data
  
  # Select cluster stars (flexible grepl)
  cluster_stars <- dfs[grepl(gsub("[-– ]", "[-– ]", cluster_name), dfs$cluster, ignore.case = TRUE), ]
  if(nrow(cluster_stars) == 0) stop(paste("No stars found for", cluster_name))
  
  # Plot main stars
  plot(data_s$l[data_s$cluster == "Other"], 
       data_s$pml[data_s$cluster == "Other"] * 4.74047,
       col = "#80808050",  # semi-transparent grey
       pch = 18, cex = cex,
       xlab = expression(italic(l) ~ "(deg)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       xlim = c(max(cluster_stars$l) + 0.5, min(cluster_stars$l) - 0.5),
       ylim = c(min(cluster_stars$pml * 4.74047) - 10, max(cluster_stars$pml * 4.74047) + 15),
       main = paste(cluster_name, "Zoom-In"),
       xaxs = "r", yaxs = "r")
  
  # Plot main cluster stars
  points(cluster_stars$l, cluster_stars$pml * 4.74047,
         pch = 16, col = cluster_colors[cluster_name], cex = cex)
  
  
  # Add model lines
  lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = 3)  # Singh
  lines(res_b$l, res_b$mul, col = "blue",    lwd = lwd, lty = 4)  # Bovy
  lines(res_g$l, res_g$mul, col = "green",   lwd = lwd, lty = 1)  # Glimpse
  lines(res_k$l, res_k$mul, col = "red",     lwd = lwd, lty = 2)  # KYSO
  
  # Grid and axes
  # grid()
  axis(side = 3, labels = FALSE, tck = -0.02)
  axis(side = 4, labels = FALSE, tck = -0.02)
  
  # Find y position of cluster stars
  y_vals <- cluster_stars$pml * 4.74047
  x_vals <- cluster_stars$l
  
  # ----- Legend placement -----
  if (is.null(legend_pos)) {
    corners <- c("topleft","topright","bottomleft","bottomright")
    x_mid <- mean(range(x_vals, na.rm = TRUE))
    y_mid <- mean(range(y_vals, na.rm = TRUE))
    quad_counts <- c(
      sum(x_vals <= x_mid & y_vals >= y_mid, na.rm = TRUE),
      sum(x_vals >  x_mid & y_vals >= y_mid, na.rm = TRUE),
      sum(x_vals <= x_mid & y_vals <  y_mid, na.rm = TRUE),
      sum(x_vals >  x_mid & y_vals <  y_mid, na.rm = TRUE)
    )
    legend_pos <- corners[which.min(quad_counts)]
  }
  
  legend(legend_pos,
         legend = c(cluster_name, "Gaia data", "This works Model", "Bovy Model", "Glimpse Model", "KYSO Model"),
         col = c(cluster_colors[cluster_name], "#80808080", "magenta", "blue", "green", "red"),
         lwd = c(NA, NA, 2, 2, 2, 2),
         lty = c(NA, NA, 3, 4, 1, 2),
         pch = c(16, 18, NA, NA, NA, NA),
         bty = "n", cex = cex-0.2)
  
  # Close the device if saving
  if (save_plot) {
    dev.off()
    message("Saved plot: ", file_name)
  }
}

library(RColorBrewer)

cols <- brewer.pal(4, "Set2")  # 4 muted colors

cluster_colors <- setNames(
  adjustcolor(cols, alpha.f = 0.6),
  c("M17", "IC 405/410/417", "Rosetta Nebula")
)


# # Set cluster colors
# set.seed(123)
# cluster_colors <- setNames(adjustcolor(rainbow(5), alpha.f = 0.4), c("M17", "IC 405/410/417","Rosetta Nebula"))

# Plot zoom-ins
plot_cluster_zoom("M17",  dist_label =   "255", cluster_colors, legend_pos = "topleft")
plot_cluster_zoom("IC 405/410/417",dist_label =  "255", cluster_colors, legend_pos = "bottomleft")
# plot_cluster_zoom("Rosetta Nebula",dist_label =  "255", cluster_colors, legend_pos = "bottomleft")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # #

# plot_cluster_zoom <- function(cluster_name, dist_label, cluster_colors, save_plot = TRUE, outdir = "/Users/ssingh/Documents/PhD/R/plots/zoomins/") {
plot_cluster_zoom <- function(cluster_name,
                              dist_label,
                              distance_ranges,
                              cluster_colors,
                              legend_pos = NULL,   # <-- new argument
                              save_plot = FALSE,
                              outdir   = "/Users/ssingh/Documents/PhD/R/plots/zoomins/") {  
    # Create output directory if saving
    if (save_plot && !dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    
    # File name for saving
    if (save_plot) {
      file_name <- paste0(outdir, gsub("[[:space:]/]", "_", cluster_name), "_zoom.png")
      png(file_name, width = 1200, height = 1200, res = 200)
    }
  
  # Reset plot
  par(mfrow = c(1,1), xpd = FALSE)
  cex=1.2
  # Extract distance range results
  res_s <- resultss[[dist_label]]  # Singh
  res_b <- resultsb[[dist_label]]  # Bovy
  res_g <- resultsg[[dist_label]]  # Glimpse
  res_k <- resultsk[[dist_label]]  # KYSO
  data_s <- res_s$data  # GAIA data
  
  # Select cluster stars (flexible grepl)
  cluster_stars <- dfs[grepl(gsub("[-– ]", "[-– ]", cluster_name), dfs$cluster, ignore.case = TRUE), ]
  if(nrow(cluster_stars) == 0) stop(paste("No stars found for", cluster_name))

  
  plot(data_s$l[data_s$cluster == "Other"], 
       data_s$pml[data_s$cluster == "Other"] * 4.74047,
       col = "#80808050",  # semi-transparent grey
       pch = 18, cex = cex,
       xlab = expression(italic(l) ~ "(deg)"),
       ylab = expression(mu[l] ~ "(km/s/kpc)"),
       xlim = c(max(cluster_stars$l) + 0.5, min(cluster_stars$l) - 0.5),
       ylim = c(min(cluster_stars$pml * 4.74047) - 10, max(cluster_stars$pml * 4.74047) + 15),
       main = paste(cluster_name, "Zoom-In"),
       xaxs = "r", yaxs = "r")
  
  points(cluster_stars$l, cluster_stars$pml * 4.74047,
         pch = 16, col = cluster_colors[cluster_name], cex = cex)
  
  
  # # Add model lines
  lines(res_s$l, res_s$mul, col = "magenta", lwd = lwd, lty = 3)  # Singh
  lines(res_b$l, res_b$mul, col = "blue",    lwd = lwd, lty = 4)  # Bovy
  lines(res_g$l, res_g$mul, col = "green",   lwd = lwd, lty = 1)  # Glimpse
  lines(res_k$l, res_k$mul, col = "red",     lwd = lwd, lty = 2)  # KYSO
# 
#     res <- resultss[[distance_ranges]]  # Singh
#     reb <- resultsb[[distance_ranges]]  # Bovy
#     reg <- resultsg[[distance_ranges]]  # Glimpse
#     rek <- resultsk[[distance_ranges]]  # KYSO
# 
#     # Add model lines
#     lines(res$l, res$mul, col = "magenta", lwd = 4, lty = 3, xpd = FALSE)
#     lines(reb$l, reb$mul, col = "blue", lwd = 4, lty = 4, xpd = FALSE)
#     lines(reg$l, reg$mul, col = "green",   lwd = lwd, lty = 1)  # Glimpse
#     lines(rek$l, rek$mul, col = "red",     lwd = lwd, lty = 2)  # KYSO

  
  # Grid and axes
  # grid()
  axis(side = 3, labels = FALSE, tck = -0.02)
  axis(side = 4, labels = FALSE, tck = -0.02)
  
  
  # ----- Legend placement -----
  if (is.null(legend_pos)) {
    corners <- c("topleft","topright","bottomleft","bottomright")
    x_mid <- mean(range(x_vals, na.rm = TRUE))
    y_mid <- mean(range(y_vals, na.rm = TRUE))
    quad_counts <- c(
      sum(x_vals <= x_mid & y_vals >= y_mid, na.rm = TRUE),
      sum(x_vals >  x_mid & y_vals >= y_mid, na.rm = TRUE),
      sum(x_vals <= x_mid & y_vals <  y_mid, na.rm = TRUE),
      sum(x_vals >  x_mid & y_vals <  y_mid, na.rm = TRUE)
    )
    legend_pos <- corners[which.min(quad_counts)]
  }
  
  legend(legend_pos,
         legend = c(cluster_name, "Gaia data", "This works Model", "Bovy Model", "Glimpse Model", "KYSO Model"),
         col = c(adjustcolor(cluster_colors[cluster_name], alpha.f = 1.5), "#80808080", "magenta", "blue", "green", "red"),
         lwd = c(NA, NA, 2, 2, 2, 2),
         lty = c(NA, NA, 3, 4, 1, 2),
         pch = c(16, 18, NA, NA, NA, NA),
         bty = "n", cex = cex-0.2)
  
  # Close the device if saving
  if (save_plot) {
    dev.off()
    message("Saved plot: ", file_name)
  }
}

all_cols <- brewer.pal(12, "Set3")

cols <- all_cols[3:8]

cluster_colors <- setNames(
  adjustcolor(cols, alpha.f = 0.6), c("NGC 2264","Orion","IC 1396"))

# # Plot zoom-ins

plot_cluster_zoom("NGC 2264",dist_label =  "10", distance_ranges="6", cluster_colors, legend_pos = "topright")
# plot_cluster_zoom("Orion", dist_label = "5", cluster_colors, legend_pos = "bottomright")
# plot_cluster_zoom("IC 1396", dist_label = "10", cluster_colors, legend_pos = "bottomleft")

# 
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # #
# # pdf("~/Documents/PhD/R/plots/MS_pmb_l.pdf", width = 11.69, height = 8.27)
# png("~/Documents/PhD/R/plots/MS_pmb_l.png",width = 17, height = 10, units = "in", res = 300)
# png("~/Documents/PhD/R/plots/full_data_mub-with-clusters.png",width = 10, height = 12, units = "in", res = 300)

# distance_ranges <- list(
#   list(d_min = 0, d_max = 200, label = "2", ylim = c(-100, 40)),
#   list(d_min = 200, d_max = 500, label = "5", ylim = c(-100, 40)),
#   list(d_min = 500, d_max = 750, label = "7", ylim = c(-50, 30)),
#   list(d_min = 750, d_max = 1000, label = "10", ylim = c(-50, 30)),
#   list(d_min = 1000, d_max = 1200, label = "125", ylim = c(-50, 30)),
#   list(d_min = 1200, d_max = 2000, label = "255", ylim = c(-50, 30))
# )
# 
# # Set up a 3x3 grid layout for plots
# par(mfrow = c(3, 2), mar = c(5, 5, 3, 2), bg = "white", cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = FALSE)
# 
# 
# for (i in seq_along(distance_ranges)) {
#   range <- distance_ranges[[i]]
#   label <- range$label
#   d_min <- range$d_min
#   d_max <- range$d_max
#   ylim  <- range$ylim
#   
#   # Extract data and model results
#   res_s <- resultss[[label]]  # Singh
#   res_b <- resultsb[[label]]  # Bovy
#   res_g <- resultsg[[label]]  # Glimpse
#   res_k <- resultsk[[label]]  # KYSO
#   
#   data_s <- res_s$data  # GAIA data
#   
#   # Plot setup
#   plot_title <- paste0(d_min, " < d < ", d_max, " pc, Mean Plx: ", round(res_s$mplx, 2))
#   
#   plot(data_s$l, data_s$pml * 4.74047, type = "n",
#        xlab = expression(italic(l) ~ "(degrees)"),
#        ylab = expression(mu[b] ~ "(km/s/kpc)"),
#        main = plot_title,
#        xlim = c(360, 0),
#        ylim = ylim,
#        tck = -0.02,  # Tick marks on all sides
#        xaxs = "r", yaxs = "r"
#   )
#   
#   axis(side = 3, labels = FALSE, tck = -0.02)
#   axis(side = 4, labels = FALSE, tck = -0.02)
#   
#   
#   # Get unique clusters (excluding "Other")
#   unique_clusters <- unique(data_s$cluster[data_s$cluster != "Other"])  # Exclude "Other"
#   
#   # Generate random colors and pch values for each cluster
#   set.seed(123)
#   cluster_colors <- if (length(unique_clusters) > 0) setNames(adjustcolor(rainbow(length(unique_clusters)), alpha.f = 0.4), unique_clusters) else character(0)
#   cluster_pchs <- setNames(sample(1:25, length(unique_clusters), replace = TRUE), unique_clusters) 
#   
#   # Plot the data points with 'Other' in grey
#   points(data_s$l, data_s$pmb * 4.74047, 
#          col = ifelse(data_s$cluster == "Other", "#80808030", "transparent"), 
#          pch = 16, cex = 0.85)
#   
#   # Plot each cluster in a different color (except "Other")
#   for (cl_name in unique_clusters) {
#     cluster_data <- data_s[data_s$cluster == cl_name, ]
#     points(cluster_data$l, cluster_data$pmb * 4.74047, 
#            col = cluster_colors[cl_name],  
#            pch = cluster_pchs[cl_name], 
#            cex = 0.85)
#   }
#   # Add model lines
#   lines(res_s$l, res_s$mub, col = "magenta", lwd = lwd, lty = "1342")  # Singh - dashed
#   lines(res_b$l, res_b$mub, col = "blue", lwd = lwd, lty = 4)     # Bovy - solid
#   lines(res_g$l, res_g$mub, col = "green", lwd = lwd, lty = 1)  # Glimpse - dotted
#   lines(res_k$l, res_k$mub, col = "red", lwd = lwd, lty = "aa")          # KYSO - dotdash
#   
#   # Grid
#   grid()
#   
#   
#   # Add legend to first plot only
#   # if (i == 1) {
#   legend("topleft",
#          legend = c("GAIA Data", "This work Model", "Bovy Model", "Glimpse Model", "KYSO Model",unique_clusters),
#          col = c("grey", "magenta", "navy", "green", "red", rep(NA, length(unique_clusters))),
#          lwd = c(NA, 2, 2, 2, 2, rep(NA, length(unique_clusters))),
#          lty = c(NA, 3, 4, 1, 2, rep(NA, length(unique_clusters))),
#          pch = c(16, NA, NA, NA, NA, cluster_pchs[unique_clusters]),
#          bty = "n", cex=cex-0.2)
#   # }
# }

# dev.off()










