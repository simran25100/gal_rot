library(mclust)
library(sandwich)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(data.table)
library(REdaS)  # For degrees to radians

calculate_proper_motion_l <- function(A, B, C, K, l, b, pi, u0, v0, w0) {
  library(REdaS)
  # Convert degrees to radians for calculations
  l_rad <- deg2rad(l)
  b_rad <- deg2rad(b)
  
  # Calculate proper motion
  mul <- (A * cos(2 * l_rad) - C * sin(2 * l_rad) + B) * cos(b_rad) + pi * (u0 * sin(l_rad) - v0 * cos(l_rad))
  mub <- -(A * sin(2 * l_rad) + C * cos(2 * l_rad) + K) * sin(b_rad) * cos(b_rad) + pi * ((u0 * cos(l_rad) + v0 * sin(l_rad)) * sin(b_rad) - w0 * cos(b_rad))
  
  # Derivatives of the proper motions
  dmul <- (mul - pi * (u0 * sin(l_rad) + v0 * cos(l_rad))) / cos(b_rad)
  dmub <- -(mub - pi * ((u0 * cos(l_rad) + v0 * sin(l_rad)) * sin(b_rad) - w0 * cos(b_rad))) / (sin(b_rad) * cos(b_rad))
  
  # Return all values as a list
  return(list(mul = mul, mub = mub, dmul = dmul, dmub = dmub))
}

file_path <- '/beegfs/car/bxster25/oc-sscu/gaia18_subset/gaia_211025_lb-pmlb-nueff-plxbias.csv'

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


pdf("plots-mclust_ABCKUVW_plots.pdf", width = 12, height = 8)  # adjust size as needed

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
  mul.mclust <- Mclust(features, G = 1:50)
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

X <- data.frame(
  A_l =  cos(b) * cos(2*l),
  C_l = -cos(b) * sin(2*l),
  B   =  cos(b),
  A_b = -sin(b) * cos(b) * sin(2*l),
  C_b = -sin(b) * cos(b) * cos(2*l),
  K   = -sin(b) * cos(b),
  par_u = parallax * sin(l),
  par_v = -parallax * cos(l),
  par_u_b = parallax * cos(l) * sin(b),
  par_v_b = parallax * sin(l) * sin(b),
  par_w_b = -parallax * cos(b),
  cluster = factor(df$cluster)
)


cluster = factor(df$cluster)

lm_l <- lm(mu_l ~ 0 + A_l + C_l + B + par_u + par_v + cluster, data = X)
lm_b <- lm(mu_b ~ 0 + A_b + C_b + K + par_u_b + par_v_b + par_w_b + cluster, data = X)


robust_l <- coeftest(lm_l, vcov = sandwich)
robust_b <- coeftest(lm_b, vcov = sandwich)
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
paste0("A <- ", round(A_est, 3), " pm ", round(A_est_err, 3))
paste0("B <- ", round(coef(lm_l)["B"], 4), " pm ", round(robust_se_l["B"], 3))
paste0("C <- ", round(C_est, 3), " pm ", round(C_est_err, 3))
paste0("K <- ", round(coef(lm_b)["K"], 3), " pm ", round(robust_se_b["K"], 3))

paste0("u <- ", round(u, 3), " pm ", round(u_err, 3))
paste0("v <- ", round(v, 3), " pm ", round(v_err, 3))
paste0("w <- ", round(coef(lm_b)["par_w_b"], 4), " pm ", round(robust_se_b["par_w_b"], 3))
