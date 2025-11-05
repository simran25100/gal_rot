library(dplyr)
library(data.table)
library(rstan)
library(REdaS)  # For degrees to radians
# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - 

# Code to run 5k data SUBSET on the cluster with 4 chains  
# Using the plx paramater model. Eps and parallax are VECTORS with all other params = real

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - 
file_path <- "/beegfs/car/bxster25/oc-sscu/gaia18_subset/subset_2.csv" 
df <- fread(file_path)

options(mc.cores = 4)  #uses 4 cores ( core per chain)

# Prepare data for Stan
stan_data <- list(
  mu_l = df$pml * 4.74047,
  mu_b = df$pmb * 4.74047,
  sigma_mul = df$pml_error * 4.74047,
  sigma_mub = df$pmb_error * 4.74047,
  l = deg2rad(df$l),
  b = deg2rad(df$b),
  parallax = df$plx_corr,
  sigma_parallax = df$parallax_ext_uncert,
  N = nrow(df)
)

#####################################################################
stan_model <- "
data {
  int<lower=1> N;               // Number of observations
  vector[N] mu_l;               // Proper motion in l
  vector[N] mu_b;               // Proper motion in b
  vector[N] sigma_mul;          // Error in mu_l
  vector[N] sigma_mub;          // Error in mu_b
  vector[N] l;                  // Galactic longitude in radians
  vector[N] b;                  // Galactic latitude in radians
  vector[N] parallax;           // Parallax
  vector[N] sigma_parallax;     // Error in parallax
}

parameters {
  real A;                            // Oort's constant A
  real B;                            // Oort's constant B
  real C;                            // Oort's constant C
  real K;                            // Oort's constant K
  real u0;                           // Solar motion component u0
  real v0;                           // Solar motion component v0
  real w0;                           // Solar motion component w0
  real<lower=0> nu_l;                // Degrees of freedom for eps_l
  real<lower=0> nu_b;                // Degrees of freedom for eps_b
  real<lower=0> s_l;                 // Scale parameter for eps_l
  real<lower=0> s_b;                 // Scale parameter for eps_b
  vector[N] eps_l;                   // Dispersion term for longitude  **vector
  vector[N] eps_b;                   // Dispersion term for latitude   **vector
  vector[N] plx;                     // Transformed parallax variable
}

model {
  // Priors
  A ~ normal(10, 3);
  B ~ normal(-10, 3);
  C ~ normal(0, 10);
  K ~ normal(0, 10); 
  u0 ~ normal(10, 3);
  v0 ~ normal(10, 3);
  w0 ~ normal(7, 3);
  s_l ~ exponential(0.05);
  s_b ~ exponential(0.05);
  nu_l ~ exponential(0.05);
  nu_b ~ exponential(0.05);

  // Components of mu_l and mu_b
  vector[N] f1_mean = (A * cos(2 * l) - C * sin(2 * l) + B) .* cos(b);
  vector[N] g1_mean = parallax .*(u0 * sin(l) - v0 * cos(l));
  
  vector[N] f2_mean = -(A * sin(2 * l) + C * cos(2 * l) + K) .* sin(b) .* cos(b);
  vector[N] g2_mean = parallax .*((u0 * cos(l) + v0 * sin(l)) .* sin(b) - w0 * cos(b));
  
  vector[N] sf_plx_l = sigma_parallax .*(u0 * sin(l) - v0 * cos(l));
  vector[N] sf_plx_b = sigma_parallax .*((u0 * cos(l) + v0 * sin(l)) .* sin(b) - w0 * cos(b));

  // Sample `eps_l` and `eps_b` using the student_t distribution / vel disp component 
  eps_l ~ student_t(nu_l, 0, s_l);  
  eps_b ~ student_t(nu_b, 0, s_b); 
  
  // Likelihood for p
  plx ~ normal(parallax, sigma_parallax);
  
  // Compute total dispersion for mu_l and mu_b
  vector[N] s_total_l = sqrt(square(sigma_mul) + square(sf_plx_l));  //
  vector[N] s_total_b = sqrt(square(sigma_mub) + square(sf_plx_b));  //
  
  // Likelihood for mu_l and mu_b
    mu_l ~ normal(f1_mean + g1_mean + plx .* eps_l, s_total_l);
    mu_b ~ normal(f2_mean + g2_mean + plx .* eps_b, s_total_b);
}
"
#####################################################################
start.time <- Sys.time()

fit <- stan(
  model_code = stan_model, 
  data = stan_data, 
  chains = 4,
  cores = 4,
  iter = 12000,
  control = list(
    adapt_delta = 0.94,
    max_treedepth = 14
  )
)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

cat("Sample size:", nrow(df), "\n")

pars = c("A", "B", "C", "K", "u0", "v0", "w0", "s_l", "s_b", "nu_l", "nu_b")
print(fit, pars = pars)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste0("/home2/bxster25/traceplots-sscu/ncirc/traceplot_2_", timestamp, ".pdf")
pdf(filename, width = 8, height = 6)
print(traceplot(fit, pars = pars))
dev.off()

save(fit, file = "/beegfs/car/bxster25/oc-sscu/saveimage/_2.Rdata")

samples_list <- extract(fit)

A <- mean(samples_list$A)
B <- mean(samples_list$B)
C <- mean(samples_list$C)
K <- mean(samples_list$K)
u0 <- mean(samples_list$u0)
v0 <- mean(samples_list$v0)
w0 <- mean(samples_list$w0)
s_l <- mean(samples_list$s_l)
s_b <- mean(samples_list$s_b)
nu_l <- mean(samples_list$nu_l)
nu_b <- mean(samples_list$nu_b)

cat("A <-", A, "\n")
cat("B <-", B, "\n")
cat("C <-", C, "\n")
cat("K <-", K, "\n")
cat("u0 <-", u0, "\n")
cat("v0 <-", v0, "\n")
cat("w0 <-", w0, "\n")
cat("s_l <-", s_l, "\n")
cat("s_b <-", s_b, "\n")
cat("nu_l <-", nu_l, "\n")
cat("nu_b <-", nu_b, "\n")

cat("scale t dist mul:", signif(s_l, 4),"\n")
cat("scale t dist mub:", signif(s_b, 4),"\n")