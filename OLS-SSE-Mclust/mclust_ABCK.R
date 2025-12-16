library(mclust)
source("~/Documents/PhD/R/calculate_proper_motion_l.R")
library(sandwich)
library(lmtest)
library(MASS)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(data.table)
library(REdaS)  # For degrees to radians


file_path <- '/Users/ssingh/Documents/PhD/cat_unique_ss/gaia_211025_lb-pmlb-nueff-plxbias.csv'
dffp <- fread(file_path)   
df <- dffp[complete.cases(pml, pmb, pml_error, pmb_error, l, b, plx_corr, parallax_ext_uncert)]

mu_l = df$pml * 4.74047
mu_b = df$pmb * 4.74047
sigma_mul = df$pml_error * 4.74047
sigma_mub = df$pmb_error * 4.74047
l = deg2rad(df$l)
b = deg2rad(df$b)
parallax = df$plx_corr
sigma_parallax = df$parallax_ext_uncert
N = nrow(df)


# Define gaia conditions
cond1_s <- (df$b > -30) & (df$b < 30)
cond2_s <- df$plx_corr > 0.5         #< (1/0.75)
cond3_s <- df$amax < 0.5
cond4_s <- df$epsi <= 1
cond5_s <- df$RUWE <= 1.4

# Define distance ranges in parsecs
dist_ranges_s <- list(
  "2"  = list(cond = (df$dist_corr >= 0)    & (df$dist_corr < 200), ylim = c(-170, 150)),
  "5"  = list(cond = (df$dist_corr >= 200)  & (df$dist_corr < 500), ylim = c(-80, 55)),
  "7"  = list(cond = (df$dist_corr >= 500)  & (df$dist_corr < 750), ylim = c(-70, 40)),
  "10" = list(cond = (df$dist_corr >= 750)  & (df$dist_corr < 1000), ylim = c(-50, 30)),
  "125" = list(cond = (df$dist_corr >= 1000) & (df$dist_corr < 1200), ylim = c(-50, 30)),
  "255" = list(cond = (df$dist_corr >= 1200) & (df$dist_corr < 2000), ylim = c(-50, 30))
)

# Singh (GAIA best-fit)
A_s <- 15.41
A_s_err <- 0.18

B_s <- -14.24
B_s_err <- 0.13

C_s <- 2.90
C_s_err <- 0.24

K_s <- 7.97
K_s_err <- 1.20

u0_s <- 9.37
u0_s_err <- 0.19

v0_s <- 11.65
v0_s_err <- 0.20

w0_s <- 7.31
w0_s_err <- 0.09



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

resultss <- build_model_results(df, cond2_s & cond3_s & cond4_s & cond5_s, dist_ranges_s, A_s, B_s, C_s, K_s, u0_s, v0_s, w0_s)



mu_l = df$pml * 4.74047
mu_b = df$pmb * 4.74047
sigma_mul = df$pml_error * 4.74047
sigma_mub = df$pmb_error * 4.74047
l = deg2rad(df$l)
b = deg2rad(df$b)
parallax = df$plx_corr
sigma_parallax = df$parallax_ext_uncert
N = nrow(df)


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
par(mfrow = c(2, 4), mar = c(5, 5, 3, 2), bg = "white", cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = FALSE)


pdf("mclust_ABCKUVW_plots_github.pdf", width = 12, height = 8)  # adjust size as needed

df$cluster <- NA
cluster_offset <- 0

for (i in seq_along(distance_ranges)) {
  
  range <- distance_ranges[[i]]
  label <- range$label
  d_min <- range$d_min
  d_max <- range$d_max
  ylim  <- range$ylim
  
  # Extract data and model results
  res_s <- resultss[[label]]  # Singh
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
  
  mu_l_sub <- data_s$pml * 4.74047    # from data_s
  
  points(data_s$l, mu_l_sub,
         col = "#80808030", pch = 16, cex = 1)
  
  features <- cbind(data_s$l, mu_l_sub, data_s$dist_corr)
  mul.mclust <- Mclust(features, G = 1:20)
  # plot(mul.mclust,ylim = ylim, xlim = c(360, 0))
  df$cluster[df$dist_corr >= d_min & df$dist_corr < d_max] <- mul.mclust$classification
  
  plot(mul.mclust, what = "BIC",
       main = paste("BIC — Bin", label))
  
  plot(data_s$l, mu_l_sub,
       pch = 16, cex = 0.6,
       col = mul.mclust$classification + 1,
       main = paste("µ_l clust class — Bin", label),
       xlab = "l (deg)", ylab = "µ_l",
       ylim = ylim, xlim = c(360, 0))
}

# Close PDF
dev.off()
# df$cluster <- factor(df$cluster)
df$cluster = factor(df$cluster)

# Longitude design matrix
X_l <- data.frame(
  mu_l =  mu_l,
  A_l  =  cos(b) * cos(2*l),
  C_l  = -cos(b) * sin(2*l),
  B    =  cos(b),
  par_u =  parallax * sin(l),
  par_v = -parallax * cos(l)
)

# Latitude design matrix
X_b <- data.frame(
  mu_b =  mu_b,
  A_b  = -sin(2*l) * sin(b) * cos(b),
  C_b  = -cos(2*l) * sin(b) * cos(b),
  K    = -sin(b) * cos(b),
  par_u_b =  parallax * cos(l) * sin(b),
  par_v_b =  parallax * sin(l) * sin(b),
  par_w_b = -parallax * cos(b)
)


lm_l <- lm(mu_l ~ 0 + A_l + C_l + B + par_u + par_v, data = X_l)
lm_b <- lm(mu_b ~ 0 + A_b + C_b + K + par_u_b + par_v_b + par_w_b, data = X_b)


library(sandwich)
library(lmtest)
cluster = factor(!is.na(df$cluster))

# Cluster-robust covariance matrices
vcov_l <- vcovCL(lm_l, cluster = cluster)
vcov_b <- vcovCL(lm_b, cluster = cluster)

# Coefficient tables with clustered SEs
robust_l <- coeftest(lm_l, vcov = vcov_l)
robust_b <- coeftest(lm_b, vcov = vcov_b)

robust_se_l <- robust_l[, "Std. Error"]
robust_se_b <- robust_b[, "Std. Error"]


# print("µ_l model for OLS:")
# print(ols_l)
print("µ_l model for OLS+SSE:")
print(robust_l)

# print("µ_b model for OLS:")
# print(ols_b)
print("µ_b model for OLS+SSE:")
print(robust_b)

# ~ ~ ~ take the av of mu_l and mu_b for A and C, u and v
A_est <- mean(c(coef(lm_l)["A_l"], coef(lm_b)["A_b"]))
A_est_err <- mean(c(robust_se_l["A_l"], robust_se_b["A_b"]))
C_est <- mean(c(coef(lm_l)["C_l"], coef(lm_b)["C_b"]))
C_est_err <- mean(c(robust_se_l["C_l"], robust_se_b["C_b"]))

u <- mean(c(coef(lm_l)["par_u"], coef(lm_b)["par_u_b"]))
u_err <- mean(c(robust_se_l["par_u"], robust_se_b["par_u_b"]))
v <- mean(c(coef(lm_l)["par_v"], coef(lm_b)["par_v_b"]))
v_err <- mean(c(robust_se_l["par_v"], robust_se_b["par_v_b"]))

# ~ ~ ~ print the values 
cat(paste0("A = ", round(A_est, 3), " ± ", round(A_est_err, 3), "\n"))
cat(paste0("B = ", round(coef(lm_l)["B"], 3), " ± ", round(robust_se_l["B"], 3), "\n"))
cat(paste0("C = ", round(C_est, 3), " ± ", round(C_est_err, 3), "\n"))
cat(paste0("K = ", round(coef(lm_b)["K"], 3), " ± ", round(robust_se_b["K"], 3), "\n"))

cat(paste0("u = ", round(u, 3), " ± ", round(u_err, 3), "\n"))
cat(paste0("v = ", round(v, 3), " ± ", round(v_err, 3), "\n"))
cat(paste0("w = ", round(coef(lm_b)["par_w_b"], 3)," ± ", round(robust_se_b["par_w_b"], 3), "\n"))

